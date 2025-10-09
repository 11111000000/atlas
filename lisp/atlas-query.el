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
      (setq pairs (seq-take (seq-sort-by #'cdr #'> pairs) k))
      (atlas-log :debug "query: candidates=%d returned=%d" (hash-table-count scores) (length pairs))
      (mapcar
       (lambda (p)
         (let* ((id (car p))
                (score (cdr p))
                (sym (atlas-model-get-symbol state id))
                (rel (and sym (plist-get sym :file)))
                (beg (or (and sym (plist-get sym :beg)) 0))
                (end (or (and sym (plist-get sym :end)) 0)))
           (list :type 'symbol
                 :id id :score score
                 :file rel :range (cons beg end)
                 :name (plist-get sym :name)
                 :sig (plist-get sym :sig)
                 :doc1 (plist-get sym :doc1))))
       pairs))))

(provide 'atlas-query)

;;; atlas-query.el ends here
