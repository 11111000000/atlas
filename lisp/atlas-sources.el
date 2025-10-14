;;; atlas-sources.el --- Provider registry and runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Providers register with capabilities and a function with signature:
;; (FN :root ROOT :changed PATHS :emit EMIT :done DONE :opts OPTS)
;; EMIT receives an alist: (:files LIST) (:symbols LIST) (:edges LIST) (:summaries LIST) (:facts LIST)

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'atlas-log)

(atlas-log :info "sources:module loaded")

(defvar atlas--sources nil
  "List of registered sources: (NAME . PLIST) where PLIST includes
:fn, :capabilities, :cost.")

(cl-defun atlas-register-source (name &key capabilities fn cost)
  "Register provider NAME with CAPABILITIES, FN, and COST."
  (let ((entry (list :capabilities capabilities :fn fn :cost (or cost 1.0))))
    (setq atlas--sources (assq-delete-all name atlas--sources))
    (push (cons name entry) atlas--sources)
    (atlas-log :info "register-source: %S caps=%S cost=%s (total=%d)"
               name capabilities (or cost 1.0) (length atlas--sources))
    name))

(cl-defun atlas-run-sources (root &key changed opts emit done kinds levels languages)
  "Run all registered sources for ROOT with optional filters.
Filters: KINDS, LEVELS, LANGUAGES. Providers must match intersection."
  (let* ((emit (or emit (lambda (_batch) nil)))
         (done (or done (lambda () nil)))
         (total (length atlas--sources))
         (matched 0)
         (invoked 0))
    (atlas-log :info "run-sources: root=%s changed=%S filters: langs=%S kinds=%S levels=%S total=%d"
               root changed languages kinds levels total)
    (dolist (entry atlas--sources)
      (let* ((name (car entry))
             (pl (cdr entry))
             (fn (plist-get pl :fn))
             (caps (plist-get pl :capabilities))
             (cap-langs (plist-get caps :languages))
             (cap-kinds (plist-get caps :kinds))
             (cap-levels (plist-get caps :levels))
             (ok t))
        (atlas-log :trace "run-sources: consider %S caps=%S" name caps)
        (when languages
          (setq ok (and ok (seq-some (lambda (x) (member x cap-langs)) languages))))
        (when (and ok kinds)
          (setq ok (and ok (seq-some (lambda (x) (member x cap-kinds)) kinds))))
        (when (and ok levels)
          (setq ok (and ok (seq-some (lambda (x) (member x cap-levels)) levels))))
        (when ok (cl-incf matched))
        (when (and ok (functionp fn))
          (cl-incf invoked)
          (atlas-log :debug "run-sources: invoke %S changed=%S" name changed)
          (condition-case err
              (funcall fn :root root :changed changed :emit emit :done done :opts opts)
            (error
             (atlas-log :error "run-sources: %S error: %S" name err))))))
    (when (= invoked 0)
      (atlas-log :warn "run-sources: no providers invoked (matched=%d total=%d)" matched total))
    (funcall done)
    (atlas-log :debug "run-sources: done matched=%d invoked=%d total=%d" matched invoked total)
    t))

(provide 'atlas-sources)

;;; atlas-sources.el ends here
