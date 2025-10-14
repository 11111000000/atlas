;;; atlas-query.el --- Lexical search and inverted index -*- lexical-binding: t; -*-

;;; Commentary:
;; Lexical search over symbol name/sig/doc1/path with simple scoring.
;; Builds and maintains token → ids index in memory via atlas-model.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'atlas-log)
(require 'atlas)
(require 'atlas-model)

(defun atlas-query--tokens (s)
  "Tokenize string S to lowercase alnum/underscore tokens."
  (atlas-model--tokens s))

(cl-defun atlas-query (root keywords &key k kinds filters)
  "Query Atlas at ROOT for KEYWORDS string.
Return list of result alists with :id :score :file :range :name :sig :doc1.
KINDS and FILTERS are reserved."
  (ignore kinds filters)
  (let* ((state (or (atlas-state root) (atlas-open root)))
         (terms (atlas-query--tokens (or keywords "")))
         (inv (atlas-model-ensure-inv-index state))
         (scores (make-hash-table :test #'equal))
         (k (or k 10)))
    (atlas-log :info "query: root=%s q=%S tokens=%d k=%d" root keywords (length terms) k)
    (when (and inv terms)
      (dolist (tkn terms)
        (let ((vec (gethash tkn inv)))
          (when vec
            (dotimes (i (length vec))
              (let* ((id (aref vec i))
                     (cur (gethash id scores 0)))
                (puthash id (1+ cur) scores)))))))
    ;; Boost by name exact/prefix matches
    (let ((pairs nil)
          (terms-set (and terms (mapcar #'identity terms))))
      (maphash
       (lambda (id score)
         (let* ((sym (atlas-model-get-symbol state id))
                (name (and sym (plist-get sym :name)))
                (down (and name (downcase name))))
           (when down
             (dolist (tkn terms-set)
               (when (string= down tkn) (cl-incf score 5))
               (when (string-prefix-p tkn down) (cl-incf score 1))))
           (push (cons id score) pairs)))
       scores)
      (setq pairs
            (seq-take
             (sort pairs
                   (lambda (a b)
                     (let ((sa (cdr a)) (sb (cdr b)))
                       (if (/= sa sb)
                           (> sa sb)
                         (let ((ia (car a)) (ib (car b)))
                           (string-lessp (format "%s" ia) (format "%s" ib)))))))
             k))
      (atlas-log :debug "query: candidates=%d returned=%d" (hash-table-count scores) (length pairs))
      (mapcar
       (lambda (p)
         (let* ((id (car p))
                (score (cdr p))
                (sym (atlas-model-get-symbol state id))
                (rel (or (and sym (plist-get sym :file))
                         ;; Fallback: derive REL from id "LANG:REL#NAME@BEG-END/KIND"
                         (and (stringp id)
                              (let* ((c (string-match ":" id))
                                     (h (and c (string-match "#" id))))
                                (when (and c h (> h (1+ c)))
                                  (substring id (1+ c) h))))))
                (beg (or (and sym (plist-get sym :beg)) 0))
                (end (or (and sym (plist-get sym :end)) 0)))
           (list (cons :type 'symbol)
                 (cons :id id) (cons :score score)
                 (cons :file rel) (cons :range (cons beg end))
                 (cons :name (and sym (plist-get sym :name)))
                 (cons :sig (and sym (plist-get sym :sig)))
                 (cons :doc1 (and sym (plist-get sym :doc1))))))
       pairs))))

(provide 'atlas-query)

;;; atlas-query.el ends here
