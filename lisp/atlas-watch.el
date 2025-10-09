;;; atlas-watch.el --- Simple watch mode for Atlas -*- lexical-binding: t; -*-

;;; Commentary:
;; Global minor-mode that watches a root directory for changes and triggers changed-only reindex.
;; Uses file-notify when available; otherwise no-op.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'atlas)
(require 'atlas-index)

(defvar atlas-watch--watches (make-hash-table :test #'equal)
  "Root → file-notify watch descriptor.")

(defun atlas-watch--rel (root path)
  "Return PATH relative to ROOT."
  (string-remove-prefix (file-name-as-directory (expand-file-name root))
                        (expand-file-name path)))

(defun atlas-watch--on-event (root event)
  "Handle file-notify EVENT for ROOT."
  (let* ((file (cadr event)))
    (when (and (stringp file) (string-match-p "\\.el\\'" file))
      (atlas-index-async root :changed (list (atlas-watch--rel root file))))))

(defun atlas-watch-start (root)
  "Start watching ROOT directory."
  (let* ((root (file-name-as-directory (expand-file-name root))))
    (unless (gethash root atlas-watch--watches)
      (when (and (fboundp 'file-notify-add-watch)
                 (file-directory-p root))
        (let ((desc (file-notify-add-watch
                     root '(change attribute-change)
                     (lambda (ev) (atlas-watch--on-event root ev)))))
          (puthash root desc atlas-watch--watches))))
    t))

(defun atlas-watch-stop (root)
  "Stop watching ROOT directory."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (desc (gethash root atlas-watch--watches)))
    (when desc
      (ignore-errors (file-notify-rm-watch desc))
      (remhash root atlas-watch--watches))
    t))

;;;###autoload
(define-minor-mode atlas-watch-mode
  "Toggle Atlas watch mode for the current default-directory."
  :global t
  :group 'atlas
  (if atlas-watch-mode
      (atlas-watch-start default-directory)
    (atlas-watch-stop default-directory)))

(provide 'atlas-watch)

;;; atlas-watch.el ends here
