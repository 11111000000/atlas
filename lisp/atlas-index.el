;;; atlas-index.el --- Index orchestration and async API -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin orchestrator over providers; exposes async wrapper and changed-only stubs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'atlas)
(require 'atlas-sources)
(require 'atlas-store)
(require 'atlas-events)

(defvar atlas--async-tasks (make-hash-table :test #'equal)
  "Root → timer object for running async index.")

(cl-defun atlas-index-async (root &key changed emit done)
  "Run indexing for ROOT asynchronously. Return plist with :token and :cancel.
CHANGED, EMIT, DONE are forwarded to providers. When finished, DONE is called."
  (let* ((token (format "atlas-%s-%f" (md5 root) (float-time)))
         (timer nil))
    (setq timer
          (run-at-time atlas-debounce-interval nil
                       (lambda ()
                         (remhash token atlas--async-tasks)
                         (condition-case err
                             (progn
                               (atlas-index root (and (eq changed :full) t))
                               (when (functionp done) (funcall done)))
                           (error (message "atlas-index-async error: %S" err))))))
    (puthash token timer atlas--async-tasks)
    (list :token token
          :cancel (lambda ()
                    (let ((tm (gethash token atlas--async-tasks)))
                      (when tm (cancel-timer tm) (remhash token atlas--async-tasks) t))))))

(defun atlas-update (root paths)
  "Update Atlas for ROOT restricted to PATHS (list of relative or absolute)."
  (interactive (list (read-directory-name "Atlas root: " nil nil t)
                     (let ((s (read-string "Paths (space-separated): ")))
                       (split-string s "[ \t]+" t))))
  (ignore root paths)
  ;; For now, run full provider; future: pass :changed=paths
  (atlas-index root nil))

(provide 'atlas-index)

;;; atlas-index.el ends here
