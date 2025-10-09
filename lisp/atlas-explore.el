;;; atlas-explore.el --- Minimal explorer and Context export -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple buffer explorer for queries and helper to export a context group.

;;; Code:

(require 'cl-lib)
(require 'atlas)
(require 'atlas-query)
(require 'atlas-plan)

;;;###autoload
(defun atlas-explore (root query &optional k)
  "Explore Atlas ROOT for QUERY and show results in a temp buffer."
  (interactive (list (read-directory-name "Atlas root: " nil nil t)
                     (read-string "Query: ")
                     (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (let* ((k (or k 20))
         (results (atlas-query root query :k k))
         (buf (get-buffer-create "*Atlas Explore*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Atlas Explore — root: %s, query: %s, k=%d\n\n" root query k))
      (dolist (r results)
        (insert (format "- %s  score=%s\n  file=%s  range=%S\n  sig=%s\n  doc1=%s\n\n"
                        (or (alist-get :name r) (alist-get :id r))
                        (alist-get :score r)
                        (alist-get :file r)
                        (alist-get :range r)
                        (or (alist-get :sig r) "")
                        (or (alist-get :doc1 r) "")))))
    (pop-to-buffer buf)))

(cl-defun atlas-build-context-group (root query &key k budget model)
  "Build a context group alist from QUERY using atlas-plan-context."
  (let* ((plan (atlas-plan-context root query :k (or k 12) :budget budget :model model)))
    (list :files (alist-get :files plan)
          :spans (alist-get :spans plan)
          :rationale (alist-get :rationale plan)
          :est-tokens (alist-get :est-tokens plan))))

(cl-defun atlas-export-to-context (group)
  "Export GROUP to Context Navigator. Placeholder returning GROUP."
  ;; Integrate with Navigator here when available.
  group)

(provide 'atlas-explore)

;;; atlas-explore.el ends here
