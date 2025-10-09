;;; atlas-index.el --- Index orchestration and async API -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin orchestrator over providers; persists batches and updates in-memory model.
;; Exposes async wrapper and changed-only stubs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'atlas-log)
(require 'atlas)
(require 'atlas-sources)
(require 'atlas-store)
(require 'atlas-events)
(require 'atlas-model)

(defvar atlas--async-tasks (make-hash-table :test #'equal)
  "Root → timer object for running async index.")

(cl-defun atlas-index-async (root &key changed emit done)
  "Run indexing for ROOT asynchronously. Return plist with :token and :cancel.
CHANGED may be :full or a list of paths (relative or absolute). When finished, DONE is called."
  (let* ((token (format "atlas-%s-%f" (md5 root) (float-time)))
         (timer nil))
    (atlas-log :info "index-async:schedule root=%s changed=%S token=%s delay=%s"
               root changed token (or atlas-debounce-interval 0.2))
    (setq timer
          (run-at-time atlas-debounce-interval nil
                       (lambda ()
                         (remhash token atlas--async-tasks)
                         (atlas-log :debug "index-async:start token=%s root=%s changed=%S" token root changed)
                         (condition-case err
                             (progn
                               (let ((arg (cond
                                           ((eq changed :full) t)
                                           ((listp changed) changed)
                                           (t nil))))
                                 (atlas-index root arg))
                               (when (functionp done) (funcall done))
                               (atlas-log :info "index-async:done token=%s" token))
                           (error
                            (atlas-log :error "index-async:error token=%s err=%S" token err)
                            (message "atlas-index-async error: %S" err))))))
    (puthash token timer atlas--async-tasks)
    (list :token token
          :cancel (lambda ()
                    (let ((tm (gethash token atlas--async-tasks)))
                      (when tm
                        (atlas-log :warn "index-async:cancel token=%s" token)
                        (cancel-timer tm)
                        (remhash token atlas--async-tasks)
                        t))))))

(defun atlas-update (root paths)
  "Update Atlas for ROOT restricted to PATHS (list of relative or absolute)."
  (interactive (list (read-directory-name "Atlas root: " nil nil t)
                     (let ((s (read-string "Paths (space-separated): ")))
                       (split-string s "[ \t]+" t))))
  ;; Route as changed-only run with given PATHS.
  (let ((changed (and (listp paths) paths)))
    (atlas-log :info "update: root=%s paths=%d" root (length changed))
    (atlas-index root changed)
    t))

(provide 'atlas-index)

;;; atlas-index.el ends here
