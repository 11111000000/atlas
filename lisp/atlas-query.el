;;; atlas-query.el --- Lexical search and inverted index -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal lexical search over symbol names/sigs/doc1.
;; Builds a token → ids index on demand in state.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'atlas)

(defvar atlas--inv-index (make-hash-table :test #'equal)
  "Root → hash-table token → vector of symbol ids.")

(defun atlas-query--tokens (s)
  "Tokenize string S to lowercase alnum/underscore tokens."
  (when (stringp s)
    (let* ((down (downcase s))
           (lst (split-string down "[^a-z0-9_]+" t)))
      lst)))

(defun atlas-query--add (h token id)
  (let ((vec (gethash token h)))
    (if vec
        (puthash token (vconcat vec (vector id)) h)
      (puthash token (vector id) h))))

(defun atlas-query--build (root)
  "Build inverted index for ROOT from in-memory symbols.
Note: placeholder uses persisted counts only; real impl should load symbols."
  (let* ((idx (make-hash-table :test #'equal)))
    ;; Placeholder: an empty index until we wire in-memory symbol table
    (puthash root idx atlas--inv-index)
    idx))

(defun atlas-query--ensure (root)
  (or (gethash root atlas--inv-index)
      (atlas-query--build root)))

(cl-defun atlas-query (root keywords &key k kinds filters)
  "Query Atlas at ROOT for KEYWORDS string.
Return list of result alists. KINDS and FILTERS reserved."
  (ignore kinds filters)
  (let* ((state (or (atlas-state root) (atlas-open root)))
         (inv (atlas-query--ensure (plist-get state :root)))
         (terms (atlas-query--tokens (or keywords "")))
         (scores (make-hash-table :test #'equal)))
    ;; Since we don't have actual symbols in memory yet, return empty.
    (when (and terms inv)
      ;; Future: iterate over tokens and merge scores
      )
    (let* ((pairs nil))
      (maphash (lambda (id score) (push (cons id score) pairs)) scores)
      (setq pairs (seq-take (seq-sort-by #'cdr #'> pairs) (or k 10)))
      (mapcar (lambda (p)
                (list :type 'symbol :id (car p) :score (cdr p)))
              pairs))))

(provide 'atlas-query)

;;; atlas-query.el ends here
