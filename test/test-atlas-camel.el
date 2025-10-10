;;; test-atlas-camel.el --- ERT tests for camelCase tokenization -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas)
(require 'atlas-model)

(ert-deftest atlas-model-camel-unicode-enabled ()
  "With unicode tokens and camel splitting: FooBar -> foobar, foo, bar."
  (let ((atlas-unicode-tokens t)
        (atlas-tokenize-camelcase t))
    (should (equal (atlas-model--tokens "FooBar")
                   '("foobar" "foo" "bar")))))

(ert-deftest atlas-model-camel-disabled ()
  "Without camel splitting (or unicode off), behavior remains original."
  ;; ASCII mode (unicode off): no camel split, just lower/ASCII
  (let ((atlas-unicode-tokens nil)
        (atlas-tokenize-camelcase t))
    (should (equal (atlas-model--tokens "FooBar")
                   '("foobar"))))
  ;; Unicode on but camel split off: still only full token
  (let ((atlas-unicode-tokens t)
        (atlas-tokenize-camelcase nil))
    (should (equal (atlas-model--tokens "FooBar")
                   '("foobar")))))

(provide 'test-atlas-camel)
