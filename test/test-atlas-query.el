;;; test-atlas-query.el --- ERT tests for atlas-query -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas-query)

(ert-deftest atlas-query-tokenization ()
  (should (equal (atlas-query--tokens "Foo_bar-123 Baz")
                 '("foo_bar" "123" "baz")))
  (should (equal (atlas-query--tokens "")
                 nil))
  (should (equal (atlas-query--tokens nil)
                 nil)))

(provide 'test-atlas-query)
