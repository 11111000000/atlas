;;; atlas-watch.el --- Simple watch mode for Atlas -*- lexical-binding: t; -*-

;;; Commentary:
;; Global minor-mode that watches a root directory for changes and triggers changed-only reindex.
;; Uses file-notify when available; otherwise no-op.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'atlas-log)
(require 'atlas)
;; Lazy-load indexing API when first used by watch.
(autoload 'atlas-index-async "atlas-index" nil nil)

(defcustom atlas-watch-file-regexp "\\.el\\'"
  "Regexp of files that trigger reindex in watch events.
Default targets Emacs Lisp; set to a broader or language-specific pattern when adding providers."
  :type 'regexp :group 'atlas)

(defvar atlas-watch--watches (make-hash-table :test #'equal)
  "Root → file-notify watch descriptor.")

(defun atlas-watch--rel (root path)
  "Return PATH relative to ROOT."
  (string-remove-prefix (file-name-as-directory (expand-file-name root))
                        (expand-file-name path)))

(defun atlas-watch--on-event (root event)
  "Handle file-notify EVENT for ROOT."
  (let* ((file (cadr event)))
    (when (and (stringp file)
               (let ((re (or (bound-and-true-p atlas-watch-file-regexp) "\\.el\\'")))
                 (string-match-p re file)))
      (atlas-log :debug "watch:event root=%s file=%s ev=%S" root file event)
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
          (puthash root desc atlas-watch--watches)
          (atlas-log :info "watch:start root=%s desc=%S" root desc))))
    t))

(defun atlas-watch-stop (root)
  "Stop watching ROOT directory."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (desc (gethash root atlas-watch--watches)))
    (when desc
      (atlas-log :info "watch:stop root=%s desc=%S" root desc)
      (ignore-errors (file-notify-rm-watch desc))
      (remhash root atlas-watch--watches))
    t))

;;;###autoload
(define-minor-mode atlas-watch-mode
  "Toggle Atlas watch mode for the current default-directory.
This affects only the current default-directory. For multiple roots,
use `atlas-watch-add-root' and `atlas-watch-remove-root'."
  :global t
  :group 'atlas
  (if atlas-watch-mode
      (atlas-watch-start default-directory)
    (atlas-watch-stop default-directory)))

;;;###autoload
(defun atlas-watch-add-root (root)
  "Start watching ROOT directory (adds to the current watch set)."
  (interactive (list (read-directory-name "Watch root: " nil nil t)))
  (atlas-watch-start root)
  (message "Atlas watch added: %s" (file-name-as-directory (expand-file-name root))))

;;;###autoload
(defun atlas-watch-remove-root (root)
  "Stop watching ROOT directory (removes from the current watch set)."
  (interactive
   (list (let* ((roots (atlas-watch-list-roots))
                (choice (completing-read "Stop watching root: " roots nil t)))
           choice)))
  (atlas-watch-stop root)
  (message "Atlas watch removed: %s" (file-name-as-directory (expand-file-name root))))

;;;###autoload
(defun atlas-watch-list-roots ()
  "Return list of currently watched roots and echo them."
  (interactive)
  (let (acc)
    (maphash (lambda (k _v) (push k acc)) atlas-watch--watches)
    (setq acc (nreverse acc))
    (when (called-interactively-p 'interactive)
      (message "Atlas watched roots: %s" (if acc (mapconcat #'identity acc ", ") "<none>")))
    acc))

(provide 'atlas-watch)

;;; atlas-watch.el ends here
