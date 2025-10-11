;;; atlas-index.el --- Index orchestration and async API -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin orchestrator over providers; persists batches and updates in-memory model.
;; Exposes async wrapper and changed-only stubs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'atlas-log)
(require 'atlas)
(require 'atlas-sources)
(require 'atlas-store)
(require 'atlas-events)
(require 'atlas-model)

(defvar atlas--async-tasks (make-hash-table :test #'equal)
  "Root → timer object for running async index.")

(defun atlas-index--excluded-dir-p (path)
  "Return non-nil if PATH is under a directory matching atlas-exclude-dirs."
  (let* ((dir (file-name-directory path))
         (components (and dir (split-string (directory-file-name dir) "/" t))))
    (seq-some
     (lambda (re)
       (seq-some (lambda (d) (string-match-p re d)) components))
     (bound-and-true-p atlas-exclude-dirs))))

(defun atlas-index--list-el-files (root)
  "List absolute .el files under ROOT respecting excludes."
  (let ((acc nil))
    (dolist (p (directory-files-recursively root "\\.el\\'" nil))
      (unless (atlas-index--excluded-dir-p p)
        (push (expand-file-name p) acc)))
    (nreverse acc)))

(defun atlas-index--rel (root path)
  "Return PATH relative to ROOT."
  (string-remove-prefix (file-name-as-directory (expand-file-name root))
                        (expand-file-name path)))

(defun atlas-index--detect-changes (root)
  "Return list of absolute paths of .el files changed since last inventory save."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (state (atlas-state root))
         (last (or (and state (plist-get state :last-index-at)) 0.0))
         (stored (ignore-errors (atlas-store-load-files root)))
         (by-rel (let ((ht (make-hash-table :test #'equal)))
                   (dolist (f stored)
                     (let ((rel (plist-get f :path))) (when rel (puthash rel f ht))))
                   ht))
         (paths (atlas-index--list-el-files root))
         (changed '()))
    (dolist (abs paths)
      (let* ((rel (atlas-index--rel root abs))
             (attr (file-attributes abs))
             (size (nth 7 attr))
             (mtime (float-time (file-attribute-modification-time attr)))
             (ctime (float-time (file-attribute-change-time attr)))
             (entry (and rel (gethash rel by-rel)))
             (prev-mtime (and entry (plist-get entry :mtime)))
             (prev-size (and entry (plist-get entry :size)))
             (prev-hash (and entry (plist-get entry :hash)))
             (need-hash? (and (bound-and-true-p atlas-hash-content)
                              (numberp size)
                              (<= size (or (bound-and-true-p atlas-max-file-size) most-positive-fixnum))))
             (cur-hash (when need-hash?
                         (ignore-errors
                           (with-temp-buffer
                             (insert-file-contents abs nil 0 size)
                             (secure-hash 'sha256 (current-buffer))))))
             (modified?
              (or (null entry)
                  (not (equal prev-size size))
                  (not (equal prev-mtime mtime))
                  (and need-hash? (not (equal prev-hash cur-hash)))
                  ;; Fallback on last-index-at: consider both mtime and ctime.
                  (and (numberp last) (or (> mtime last) (> ctime last))))))
        (when modified?
          (push abs changed))))
    (nreverse changed)))

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

;;;###autoload
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
