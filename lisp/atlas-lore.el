;;; atlas-lore.el --- lore.el integration (getter atlas) -*- lexical-binding: t; -*-

;;; Commentary:
;; Streams atlas results to lore via emit/done callbacks.

;;; Code:

(require 'cl-lib)
(require 'atlas)
(require 'atlas-query)
(require 'atlas-plan)

(cl-defun lore-getter-atlas-run (&key request topk emit done)
  "Run lore getter against Atlas. REQUEST may include :query and :root.
EMIT receives each item; DONE called once."
  (let* ((root (or (alist-get :root request) default-directory))
         (query (or (alist-get :query request) ""))
         (k (or topk 10))
         (results (atlas-query root query :k k)))
    (dolist (r results)
      (let* ((id (alist-get :id r))
             (item (list :type 'symbol
                         :title (or id "symbol")
                         :snippet ""
                         :path ""
                         :range (cons 0 0)
                         :score (or (alist-get :score r) 0.0)
                         :source 'atlas)))
        (when (functionp emit) (funcall emit item))))
    (when (functionp done) (funcall done))
    t))

(provide 'atlas-lore)

;;; atlas-lore.el ends here
