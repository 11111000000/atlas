;;; ert-runner.el --- ERT bootstrap for Atlas tests -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))

;; In batch test environments some setups mark *Messages* read-only.
;; Make all `message' calls safe and redirect output to stdout instead.
(when noninteractive
  (setq message-log-max nil)
  (defun atlas-test--safe-message-around (orig fmt &rest args)
    (let ((s (apply #'format fmt args)))
      (condition-case _err
          (princ (concat s "\n"))
        (error nil))
      ;; Do not call `orig' in batch to avoid touching *Messages*.
      s))
  (advice-add 'message :around #'atlas-test--safe-message-around))

(require 'ert)

(defun atlas-test--load-dir (dir)
  "Load all test-*.el files under DIR."
  (when (and dir (file-directory-p dir))
    (dolist (f (directory-files dir t "\\`test-.*\\.el\\'"))
      (load f nil t))))

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (t-dir (expand-file-name "../t" here)))
  ;; Load tests from current test/ and optional sibling t/
  (atlas-test--load-dir here)
  (atlas-test--load-dir t-dir))

(ert-run-tests-batch-and-exit)
