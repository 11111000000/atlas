;;; atlas-events.el --- Simple event bus for Atlas -*- lexical-binding: t; -*-

;;; Commentary:
;; Publish/subscribe with topic symbols. Handlers are exception-safe.
;; Returns unsubscribe lambdas.

;;; Code:

(require 'cl-lib)

(defvar atlas--subscriptions (make-hash-table :test #'eq)
  "Topic symbol → list of handler functions.")

(defun atlas-events-subscribe (topic fn)
  "Subscribe FN to TOPIC. Return an unsubscribe lambda."
  (let* ((lst (gethash topic atlas--subscriptions)))
    (puthash topic (cons fn lst) atlas--subscriptions))
  (lambda ()
    (let ((lst (delq fn (gethash topic atlas--subscriptions))))
      (puthash topic lst atlas--subscriptions)
      t)))

(defun atlas-events-unsubscribe (topic fn)
  "Unsubscribe FN from TOPIC."
  (let ((lst (delq fn (gethash topic atlas--subscriptions))))
    (puthash topic lst atlas--subscriptions)
    t))

(defun atlas-events-publish (topic &rest args)
  "Publish ARGS to subscribers of TOPIC."
  (let ((lst (copy-sequence (gethash topic atlas--subscriptions))))
    (dolist (fn lst)
      (condition-case err
          (apply fn args)
        (error (message "atlas-events handler error on %S: %S" topic err)))))
  t)

(provide 'atlas-events)

;;; atlas-events.el ends here
