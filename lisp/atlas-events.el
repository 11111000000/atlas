;;; atlas-events.el --- Simple event bus for Atlas -*- lexical-binding: t; -*-

;;; Commentary:
;; Publish/subscribe with topic symbols. Handlers are exception-safe.
;; Returns unsubscribe lambdas.
;; Delivery order is newest-first (latest subscription handled first).

;;; Code:

(require 'cl-lib)
(require 'atlas-log)

(defvar atlas--subscriptions (make-hash-table :test #'eq)
  "Topic symbol → list of handler functions.")

(defun atlas-events-subscribe (topic fn)
  "Subscribe FN to TOPIC. Return an unsubscribe lambda."
  (let* ((lst (gethash topic atlas--subscriptions)))
    (puthash topic (cons fn lst) atlas--subscriptions)
    (atlas-log :debug "events:subscribe topic=%S handler=%s total=%d"
               topic fn (length (gethash topic atlas--subscriptions))))
  (lambda ()
    (let ((lst (delq fn (gethash topic atlas--subscriptions))))
      (puthash topic lst atlas--subscriptions)
      (atlas-log :debug "events:unsubscribe topic=%S handler=%s total=%d"
                 topic fn (length (gethash topic atlas--subscriptions)))
      t)))

(defun atlas-events-unsubscribe (topic fn)
  "Unsubscribe FN from TOPIC."
  (let ((lst (delq fn (gethash topic atlas--subscriptions))))
    (puthash topic lst atlas--subscriptions)
    (atlas-log :debug "events:unsubscribe topic=%S handler=%s total=%d"
               topic fn (length (gethash topic atlas--subscriptions)))
    t))

(defun atlas-events-publish (topic &rest args)
  "Publish ARGS to subscribers of TOPIC."
  (let ((lst (copy-sequence (gethash topic atlas--subscriptions))))
    (atlas-log :trace "events:publish topic=%S handlers=%d args=%S" topic (length lst) args)
    (dolist (fn lst)
      (condition-case err
          (apply fn args)
        (error
         (atlas-log :error "events:handler error topic=%S fn=%s err=%S" topic fn err)
         (message "atlas-events handler error on %S: %S" topic err)))))
  t)

(provide 'atlas-events)

;;; atlas-events.el ends here
