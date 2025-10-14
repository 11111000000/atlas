;;; atlas-dump.el --- Export/import for Atlas store -*- lexical-binding: t; -*-

;;; Commentary:
;; Dump store contents to sexp/jsonl and import external batches.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'atlas)
(require 'atlas-store)
(require 'atlas-model)

(cl-defun atlas-dump (root what &key format path)
  "Dump WHAT from Atlas ROOT to PATH in FORMAT.
WHAT may be 'all or a list among (files symbols edges summaries facts inv).
FORMAT is 'sexp or 'jsonl. PATH is required."
  (unless path (user-error "atlas-dump: PATH is required"))
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (what (if (eq what 'all) '(files symbols edges summaries facts inv) what))
         (format (or format 'sexp))
         (state (or (atlas-state root) (atlas-open root)))
         (files (when (memq 'files what) (atlas-store-load-files root)))
         (symbols (when (memq 'symbols what) (atlas-store-load-symbols root)))
         (edges (when (memq 'edges what) (atlas-store-load-edges root)))
         (sums (when (memq 'summaries what) (atlas-store-load-summaries root)))
         (facts (when (memq 'facts what) (atlas-store-load-facts root)))
         (inv (when (memq 'inv what)
                (let ((ht (atlas-model-ensure-inv-index state))
                      (acc '()))
                  (maphash (lambda (k v) (push (cons k v) acc)) ht)
                  (nreverse acc)))))
    (pcase format
      ('sexp
       (with-temp-buffer
         (prin1 (list :files files :symbols symbols :edges edges :summaries sums :facts facts :inv inv) (current-buffer))
         (insert "\n")
         (write-region (point-min) (point-max) path nil 'silent)))
      ('jsonl
       (with-temp-buffer
         (dolist (pair `((files . ,files) (symbols . ,symbols) (edges . ,edges) (summaries . ,sums) (facts . ,facts) (inv . ,inv)))
           (let* ((key (car pair)) (vals (cdr pair)))
             (when vals
               (if (eq key 'inv)
                   (dolist (kv vals)
                     (let* ((obj `((type . "inv")
                                   (token . ,(car kv))
                                   (ids . ,(cdr kv)))))
                       (insert (json-serialize obj :object-type 'alist) "\n")))
                 (dolist (x vals)
                   (let ((obj `((type . ,(symbol-name key)) (data . ,x))))
                     (insert (json-serialize obj :object-type 'alist) "\n")))))))
         (write-region (point-min) (point-max) path nil 'silent)))
      (_ (user-error "atlas-dump: unknown format %S" format))))
  t)

(cl-defun atlas-import (root batch)
  "Import BATCH alist into Atlas ROOT, merging into store and in-memory model.
BATCH keys: :files, :symbols, :edges, :summaries, :facts, optional :file for per-file replace."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (state (or (atlas-state root) (atlas-open root))))
    (atlas-store-save-batch root batch)
    (require 'atlas-model)
    (atlas-model-merge-batch state batch)
    (atlas--set-state root state)
    t))

(provide 'atlas-dump)

;;; atlas-dump.el ends here
