;;; atlas-sources.el --- Provider registry and runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Providers register with capabilities and a function with signature:
;; (FN :root ROOT :changed PATHS :emit EMIT :done DONE :opts OPTS)
;; EMIT receives an alist: (:files LIST) (:symbols LIST) (:edges LIST) (:summaries LIST)

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar atlas--sources nil
  "List of registered sources: (NAME . PLIST) where PLIST includes
:fn, :capabilities, :cost.")

(cl-defun atlas-register-source (name &key capabilities fn cost)
  "Register provider NAME with CAPABILITIES, FN, and COST."
  (let ((entry (list :capabilities capabilities :fn fn :cost (or cost 1.0))))
    (setq atlas--sources (assq-delete-all name atlas--sources))
    (push (cons name entry) atlas--sources)
    name))

(cl-defun atlas-run-sources (root &key changed opts emit done kinds levels languages)
  "Run all registered sources for ROOT with optional filters.
Filters: KINDS, LEVELS, LANGUAGES. Providers must match intersection."
  (let* ((emit (or emit (lambda (_batch) nil)))
         (done (or done (lambda () nil))))
    (dolist (entry atlas--sources)
      (let* ((name (car entry))
             (pl (cdr entry))
             (fn (plist-get pl :fn))
             (caps (plist-get pl :capabilities))
             (cap-langs (plist-get caps :languages))
             (cap-kinds (plist-get caps :kinds))
             (cap-levels (plist-get caps :levels))
             (ok t))
        (when languages
          (setq ok (and ok (seq-some (lambda (x) (member x cap-langs)) languages))))
        (when (and ok kinds)
          (setq ok (and ok (seq-some (lambda (x) (member x cap-kinds)) kinds))))
        (when (and ok levels)
          (setq ok (and ok (seq-some (lambda (x) (member x cap-levels)) levels))))
        (when (and ok (functionp fn))
          (condition-case err
              (funcall fn :root root :changed changed :emit emit :done done :opts opts)
            (error (message "atlas source %S error: %S" name err))))))
    (funcall done)
    t))

(provide 'atlas-sources)

;;; atlas-sources.el ends here
