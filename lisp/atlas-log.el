;;; atlas-log.el --- Centralized logging for Atlas -*- lexical-binding: t; -*-

;;; Commentary:
;; Cheap, centralized logging with levels and capped buffer.
;; Enabled by default for debugging; disable later via custom.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup atlas-log nil
  "Logging for Atlas."
  :group 'atlas
  :prefix "atlas-log-")

(defcustom atlas-log-enabled t
  "If non-nil, enable Atlas logging."
  :type 'boolean :group 'atlas-log)

(defcustom atlas-log-level :debug
  "Minimum log level: one of :error :warn :info :debug :trace."
  :type '(choice (const :error) (const :warn) (const :info) (const :debug) (const :trace))
  :group 'atlas-log)

(defcustom atlas-log-buffer "*Atlas Log*"
  "Buffer name for Atlas logs."
  :type 'string :group 'atlas-log)

(defcustom atlas-log-max-lines 2000
  "Max number of lines kept in the log buffer."
  :type 'integer :group 'atlas-log)

(defun atlas-log--level->num (lvl)
  (pcase lvl
    (:error 0) (:warn 1) (:info 2) (:debug 3) (:trace 4)
    (_ 3)))

(defun atlas-logp (lvl)
  "Return non-nil when LVL should be logged."
  (and atlas-log-enabled
       (>= (atlas-log--level->num lvl)
           (atlas-log--level->num atlas-log-level))))

(defun atlas-log--buf ()
  (get-buffer-create atlas-log-buffer))

(defun atlas-log--trim ()
  "Trim log buffer to atlas-log-max-lines."
  (when (and (integerp atlas-log-max-lines) (> atlas-log-max-lines 0))
    (with-current-buffer (atlas-log--buf)
      (let ((lines (count-lines (point-min) (point-max))))
        (when (> lines atlas-log-max-lines)
          (goto-char (point-min))
          (forward-line (- lines (ceiling (* 0.75 atlas-log-max-lines))))
          (delete-region (point-min) (point)))))))

(defun atlas-log (lvl fmt &rest args)
  "Log message at LVL using FMT and ARGS."
  (when (atlas-logp lvl)
    (let ((ts (format-time-string "%Y-%m-%d %H:%M:%S"))
          (msg (apply #'format fmt args)))
      (with-current-buffer (atlas-log--buf)
        (goto-char (point-max))
        (insert (format "%s [%s] %s\n" ts (substring (symbol-name lvl) 1) msg))
        (atlas-log--trim)))
    (when (memq lvl '(:error :warn))
      (ignore-errors (message "Atlas %s: %s" (substring (symbol-name lvl) 1) (apply #'format fmt args)))))
  t)

;;;###autoload
(defun atlas-log-open ()
  "Open Atlas log buffer."
  (interactive)
  (pop-to-buffer (atlas-log--buf)))

;;;###autoload
(defun atlas-log-clear ()
  "Clear Atlas log buffer."
  (interactive)
  (with-current-buffer (atlas-log--buf)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (message "Atlas log cleared"))

(provide 'atlas-log)

;;; atlas-log.el ends here
