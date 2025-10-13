;;; atlas-setup.el --- Local setup helper for Atlas -*- lexical-binding: t; -*-

;; Usage examples (put one of these in your init.el/early-init.el):
;;
;;   (load "/home/az/Code/atlas/lisp/atlas-setup.el")
;;   ;; optional conveniences:
;;   ;; (atlas-setup-enable)                    ; turn on progress UI
;;   ;; (atlas-setup-enable "/home/az/Code/atlas/") ; also add root to watch
;;
;; Or for Doom/straight/etc. you can still use use-package, see
;; atlas-use-package-example.el in this directory.

;;; Code:

(defvar atlas-setup--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Absolute path to Atlas lisp directory when this file is loaded.")

(when (and atlas-setup--lisp-dir (not (member atlas-setup--lisp-dir load-path)))
  (add-to-list 'load-path atlas-setup--lisp-dir))

;; Load autoloads only (do not force modules on startup)
(require 'atlas-autoloads)

;;;###autoload
(defun atlas-setup-enable (&optional root)
  "Enable convenient Atlas defaults. Optionally add ROOT to watch.

- Turns on atlas-progress-mode if available.
- If ROOT is provided, adds it to atlas-watch (file-notify)."
  (interactive (list (when current-prefix-arg (read-directory-name "Watch root: " nil nil t))))
  (when (fboundp 'atlas-progress-mode)
    (atlas-progress-mode 1))
  (when (and root (fboundp 'atlas-watch-add-root))
    (atlas-watch-add-root root))
  (message "Atlas setup enabled%s"
           (if root (format " (watching %s)" (file-name-as-directory (expand-file-name root))) "")))

(provide 'atlas-setup)

;;; atlas-setup.el ends here
