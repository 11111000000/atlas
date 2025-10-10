;;; atlas-model.el --- In-memory model and inverted index -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-ish in-memory model: files/symbols/edges, and an inverted index.
;; Clean core + thin ports: store I/O lives in atlas-store; orchestration in atlas/atlas-index.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'atlas-store)
(require 'atlas-log)
(require 'ucs-normalize nil t)

(defvar atlas-unicode-tokens nil)
(defvar atlas-tokenize-camelcase nil)

;; Index keys inside state :indexes plist
(defconst atlas-model--k-files-idx :files-idx)
(defconst atlas-model--k-sym-by-id :symbols-by-id)
(defconst atlas-model--k-sym-by-name :symbols-by-name)
(defconst atlas-model--k-edges-out :edges-out)
(defconst atlas-model--k-edges-in :edges-in)
(defconst atlas-model--k-inv-index :inv-index)
(defconst atlas-model--k-id->tokens :id->tokens)

(defun atlas-model--get-idx (state key)
  "Get hash-table index KEY from STATE :indexes, creating if absent."
  (let* ((idx (or (plist-get state :indexes) (list)))
         (ht (plist-get idx key)))
    (unless (hash-table-p ht)
      (setq ht (make-hash-table :test #'equal))
      (setq idx (plist-put idx key ht))
      (plist-put state :indexes idx))
    ht))

(defun atlas-model--set-idx (state key value)
  "Set index KEY in STATE :indexes to VALUE."
  (let ((idx (or (plist-get state :indexes) (list))))
    (plist-put state :indexes (plist-put idx key value))))

(defun atlas-model--clear-inv (state)
  "Mark inverted index dirty in STATE."
  (atlas-model--set-idx state atlas-model--k-inv-index (make-hash-table :test #'equal))
  (atlas-model--set-idx state atlas-model--k-id->tokens (make-hash-table :test #'equal))
  (plist-put state :inv-index-ready? nil))

(defun atlas-model--push (ht key val)
  "Push VAL to list under KEY in hash-table HT."
  (let ((cur (gethash key ht)))
    (puthash key (cons val cur) ht)))

(defun atlas-model--dedup-list (lst)
  "Return LST without duplicates preserving order."
  (let ((seen (make-hash-table :test #'equal))
        (res '()))
    (dolist (x lst)
      (unless (gethash x seen)
        (puthash x t seen)
        (push x res)))
    (nreverse res)))

(defun atlas-model--as-vector (lst)
  "Return vector from LST."
  (vconcat (or lst '())))

(defun atlas-model--rebuild-edges-in (state)
  "Rebuild edges-in map from edges-out in STATE."
  (let* ((out (atlas-model--get-idx state atlas-model--k-edges-out))
         (in (make-hash-table :test #'equal)))
    (maphash
     (lambda (_from edges)
       (dolist (e edges)
         (let ((to (plist-get e :to)))
           (when to
             (atlas-model--push in to e)))))
     out)
    (atlas-model--set-idx state atlas-model--k-edges-in in)
    in))

(defun atlas-model--index-symbol (state sym)
  "Insert SYM plist into STATE symbol indexes."
  (let* ((id (plist-get sym :id))
         (name (plist-get sym :name))
         (by-id (atlas-model--get-idx state atlas-model--k-sym-by-id))
         (by-name (atlas-model--get-idx state atlas-model--k-sym-by-name)))
    (when id (puthash id sym by-id))
    (when name (atlas-model--push by-name name id))
    t))

(defun atlas-model--remove-symbols-for-file (state rel)
  "Remove all symbols whose :file equals REL from STATE."
  (let* ((by-id (atlas-model--get-idx state atlas-model--k-sym-by-id))
         (by-name (atlas-model--get-idx state atlas-model--k-sym-by-name))
         (to-del '()))
    (maphash
     (lambda (id sym)
       (when (equal (plist-get sym :file) rel)
         (push (cons id sym) to-del)))
     by-id)
    (dolist (p to-del)
      (let* ((id (car p))
             (sym (cdr p))
             (name (plist-get sym :name)))
        (remhash id by-id)
        (when name
          (let ((lst (delq id (gethash name by-name))))
            (if lst (puthash name lst by-name) (remhash name by-name))))))
    (atlas-model--clear-inv state)
    (length to-del)))

(defun atlas-model--set-edges-for-file (state rel edges)
  "Replace edges-out for REL with EDGES in STATE and rebuild edges-in."
  (let ((out (atlas-model--get-idx state atlas-model--k-edges-out)))
    (puthash rel (copy-sequence edges) out))
  (atlas-model--rebuild-edges-in state))

(defun atlas-model--set-files (state files)
  "Replace files index with FILES list in STATE."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (f files)
      (let ((path (plist-get f :path)))
        (when path (puthash path f ht))))
    (atlas-model--set-idx state atlas-model--k-files-idx ht))
  t)

(defun atlas-model--symbol-fields-for-tokens (sym)
  "Return list of strings to tokenize for SYM."
  (let ((name (plist-get sym :name))
        (sig (plist-get sym :sig))
        (doc1 (plist-get sym :doc1))
        (file (plist-get sym :file)))
    (seq-filter #'identity (list name sig doc1 file))))

(defun atlas-model--tokenize-fields (fields)
  "Tokenize and combine FIELDS (list of strings)."
  (let ((acc '()))
    (dolist (f fields)
      (setq acc (nconc acc (atlas-model--tokens f))))
    (atlas-model--dedup-list acc)))

(defun atlas-model--build-inv-index (state)
  "Build inverted index for STATE from symbols."
  (let* ((by-id (atlas-model--get-idx state atlas-model--k-sym-by-id))
         (acc (make-hash-table :test #'equal))
         (id2t (make-hash-table :test #'equal)))
    (maphash
     (lambda (id sym)
       (let* ((toks (atlas-model--tokenize-fields (atlas-model--symbol-fields-for-tokens sym))))
         (puthash id toks id2t)
         (dolist (tkn toks)
           (atlas-model--push acc tkn id))))
     by-id)
    ;; Convert posting lists to vectors
    (let ((inv (make-hash-table :test #'equal)))
      (maphash
       (lambda (tkn ids)
         (puthash tkn (atlas-model--as-vector (nreverse (atlas-model--dedup-list ids))) inv))
       acc)
      (atlas-model--set-idx state atlas-model--k-inv-index inv)
      (atlas-model--set-idx state atlas-model--k-id->tokens id2t)
      (plist-put state :inv-index-ready? t)
      inv)))

;; Public API

(defun atlas-model-get-symbol (state id)
  "Get symbol plist by ID from STATE."
  (let ((by-id (atlas-model--get-idx state atlas-model--k-sym-by-id)))
    (gethash id by-id)))

(defun atlas-model-edges-out (state key)
  "Get list of edges with :from KEY from STATE."
  (let ((out (atlas-model--get-idx state atlas-model--k-edges-out)))
    (copy-sequence (gethash key out))))

(defun atlas-model-edges-in (state key)
  "Get list of edges with :to KEY from STATE."
  (let ((in (atlas-model--get-idx state atlas-model--k-edges-in)))
    (copy-sequence (gethash key in))))

(defun atlas-model-ensure-inv-index (state)
  "Ensure inverted index exists for STATE; return it."
  (let* ((inv (plist-get (plist-get state :indexes) atlas-model--k-inv-index)))
    (if (and (hash-table-p inv) (plist-get state :inv-index-ready?))
        inv
      (atlas-model--build-inv-index state))))

(defun atlas-model--tokens (s)
  "Tokenize string S. Default: lowercase ASCII [a-z0-9_]+.
If `atlas-unicode-tokens' is non-nil, normalize (NFKC), use [[:word:]]+ on original case,
then downcase tokens; when `atlas-tokenize-camelcase' is also non-nil, split CamelCase into sub-tokens."
  (when (and (stringp s) (> (length s) 0))
    (let ((res '()))
      (if (bound-and-true-p atlas-unicode-tokens)
          ;; Unicode-aware path with optional CamelCase splitting
          (let* ((norm (if (fboundp 'ucs-normalize-NFKC-string)
                           (ucs-normalize-NFKC-string s)
                         s)))
            (with-temp-buffer
              (insert norm)
              (goto-char (point-min))
              (while (re-search-forward "[[:word:]]+" nil t)
                (let* ((raw (match-string 0))
                       (low (downcase raw)))
                  ;; Always keep the downcased full token
                  (push low res)
                  ;; Optionally add CamelCase sub-tokens (ASCII heuristic)
                  (when (bound-and-true-p atlas-tokenize-camelcase)
                    (let ((len (length raw))
                          (start 0))
                      (dotimes (i len)
                        (when (and (> i 0)
                                   (let ((prev (aref raw (1- i)))
                                         (cur  (aref raw i)))
                                     (and (>= prev ?a) (<= prev ?z)
                                          (>= cur  ?A) (<= cur  ?Z))))
                          (let ((part (substring raw start i)))
                            (when (> (length part) 0)
                              (push (downcase part) res)))
                          (setq start i)))
                      (let ((last (substring raw start)))
                        (when (> (length last) 0)
                          (push (downcase last) res)))))))))
        ;; Legacy ASCII path
        (with-temp-buffer
          (insert (downcase s))
          (goto-char (point-min))
          (while (re-search-forward "[a-z0-9_]+" nil t)
            (push (match-string 0) res))))
      (nreverse (atlas-model--dedup-list res)))))

(cl-defun atlas-model-merge-batch (state batch)
  "Merge BATCH into STATE indexes. BATCH may include :files :symbols :edges and :file REL.
Symbols and edges are replaced per file REL. Without REL, symbols are appended
and edges are appended grouped by their :from key."
  (let* ((files (alist-get :files batch))
         (symbols (alist-get :symbols batch))
         (edges (alist-get :edges batch))
         (rel (alist-get :file batch)))
    (atlas-log :trace "model:merge-batch rel=%s files=%d symbols=%d edges=%d"
               (or rel "-") (length files) (length symbols) (length edges))
    (when files
      (atlas-model--set-files state files))
    (when rel
      ;; Replace symbols for file
      (when symbols
        (atlas-model--remove-symbols-for-file state rel)
        (dolist (sym symbols)
          (atlas-model--index-symbol state sym)))
      ;; Replace edges for file
      (when edges
        (atlas-model--set-edges-for-file state rel edges)))
    (when (and (not rel) symbols)
      ;; No rel provided, just index symbols (append-only)
      (dolist (sym symbols) (atlas-model--index-symbol state sym)))
    (when (and (not rel) edges)
      ;; Append edges by :from key and rebuild edges-in
      (let ((out (atlas-model--get-idx state atlas-model--k-edges-out)))
        (dolist (e edges)
          (let ((from (plist-get e :from)))
            (when from
              (atlas-model--push out from e))))
        (atlas-model--rebuild-edges-in state)))
    (when (or symbols edges)
      ;; Edges don't affect inv-index, but keep behavior consistent
      (atlas-model--clear-inv state)))
  state)

(cl-defun atlas-model-build-from-store (state)
  "Load store for ROOT from STATE and build in-memory indexes."
  (let* ((root (plist-get state :root))
         (files (ignore-errors (atlas-store-load-files root)))
         (symbols (ignore-errors (atlas-store-load-symbols root)))
         (edges (ignore-errors (atlas-store-load-edges root))))
    (when (listp files) (atlas-model--set-files state files))
    ;; Symbols
    (let ((by-id (atlas-model--get-idx state atlas-model--k-sym-by-id))
          (by-name (atlas-model--get-idx state atlas-model--k-sym-by-name)))
      (clrhash by-id)
      (clrhash by-name)
      (dolist (sym symbols)
        (atlas-model--index-symbol state sym)))
    ;; Edges
    (let ((out (atlas-model--get-idx state atlas-model--k-edges-out)))
      (clrhash out)
      (dolist (e edges)
        (let ((from (plist-get e :from)))
          (when from (atlas-model--push out from e)))))
    (atlas-model--rebuild-edges-in state)
    (atlas-model--clear-inv state)
    state))

(provide 'atlas-model)

;;; atlas-model.el ends here
