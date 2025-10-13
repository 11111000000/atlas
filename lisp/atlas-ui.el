;;; atlas-ui.el --- Minimal UI bits: progress indicator -*- lexical-binding: t; -*-

;;; Commentary:
;; Global minor-mode showing indexing progress via atlas-events in the mode-line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'atlas-log)
(require 'atlas-events)

;;;###autoload
(defgroup atlas-ui nil
  "UI helpers for Atlas."
  :group 'atlas
  :prefix "atlas-ui-")

(defcustom atlas-ui-progress-throttle 0.2
  "Throttle for mode-line updates (seconds)."
  :type 'number :group 'atlas-ui)

(defvar atlas-ui--unsubs nil
  "Unsubscribe lambdas for progress handlers.")

(defvar atlas-ui--segment ""
  "Modeline segment text for Atlas progress.")

(defvar atlas-ui--timer nil
  "Debounce timer for updating the modeline.")

(defun atlas-ui--set-segment (s)
  (setq atlas-ui--segment s)
  (force-mode-line-update t))

(defun atlas-ui--update-debounced (text)
  (when atlas-ui--timer (cancel-timer atlas-ui--timer))
  (setq atlas-ui--timer
        (run-at-time atlas-ui-progress-throttle nil
                     (lambda () (atlas-ui--set-segment text)))))

(defun atlas-ui--plist-get (plist key)
  (plist-get plist key))

(defun atlas-ui--on-start (&rest args)
  (ignore args)
  (atlas-log :info "ui: index start")
  (atlas-ui--update-debounced "Atlas: indexing…"))

(defun atlas-ui--on-progress (&rest args)
  (let* ((files (or (atlas-ui--plist-get args :files) 0))
         (syms  (or (atlas-ui--plist-get args :symbols) 0))
         (edges (or (atlas-ui--plist-get args :edges) 0)))
    (atlas-ui--update-debounced (format "Atlas: f=%s s=%s e=%s" files syms edges))))

(defun atlas-ui--on-done (&rest _args)
  (atlas-log :info "ui: index done")
  (atlas-ui--update-debounced ""))

(defvar atlas-ui--mode-line
  '(:eval (when (and (boundp 'atlas-progress-mode) atlas-progress-mode)
            (when (and (stringp atlas-ui--segment)
                       (> (length atlas-ui--segment) 0))
              (concat " " atlas-ui--segment))))
  "Mode-line construct for Atlas progress.")

;;;###autoload
(define-minor-mode atlas-progress-mode
  "Global minor-mode to show Atlas indexing progress in mode-line."
  :global t
  :group 'atlas
  (if atlas-progress-mode
      (progn
        ;; Add to mode-line
        (unless (member 'atlas-ui--mode-line global-mode-string)
          (add-to-list 'global-mode-string 'atlas-ui--mode-line t))
        ;; Subscribe to events
        (let ((u1 (atlas-events-subscribe :atlas-index-start #'atlas-ui--on-start))
              (u2 (atlas-events-subscribe :atlas-index-progress #'atlas-ui--on-progress))
              (u3 (atlas-events-subscribe :atlas-index-done #'atlas-ui--on-done))
              (u4 (atlas-events-subscribe :atlas-index-error #'atlas-ui--on-done)))
          (setq atlas-ui--unsubs (list u1 u2 u3 u4)))
        (atlas-ui--set-segment ""))
    ;; Disable
    (dolist (u atlas-ui--unsubs) (ignore-errors (funcall u)))
    (setq atlas-ui--unsubs nil)
    (setq global-mode-string (delq 'atlas-ui--mode-line global-mode-string))
    (atlas-ui--set-segment "")))

(provide 'atlas-ui)

;;; atlas-ui.el ends here
