;;; test-atlas-events.el --- ERT tests for atlas-events -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas-events)

(ert-deftest atlas-events-subscribe-unsubscribe ()
  (let* ((topic :foo)
         (acc '())
         (fn (lambda (&rest args) (setq acc args)))
         (unsub (atlas-events-subscribe topic fn)))
    (atlas-events-publish topic 1 2 3)
    (should (equal acc '(1 2 3)))
    (funcall unsub)
    (setq acc '())
    (atlas-events-publish topic 4)
    (should (equal acc '()))))

(provide 'test-atlas-events)
