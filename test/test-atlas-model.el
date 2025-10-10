;;; test-atlas-model.el --- ERT tests for atlas-model -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas-model)

(ert-deftest atlas-model-tokens-basic ()
  "Tokenize ASCII words and underscores; ensure lowercase and dedupe."
  (should (equal (atlas-model--tokens "FooBar baz_qux42 baz")
                 '("foobar" "baz_qux42" "baz"))))

(ert-deftest atlas-model-merge-batch-per-file ()
  "Merging per file replaces symbols for that file and clears inv index."
  (let* ((state (list :root default-directory :indexes (list)))
         (batch1 (list (cons :file "lisp/a.el")
                       (cons :symbols (list (list :id "elisp:lisp/a.el#a@1-2/function"
                                                  :file "lisp/a.el" :name "a" :beg 1 :end 2)))))
         (batch2 (list (cons :file "lisp/a.el")
                       (cons :symbols (list (list :id "elisp:lisp/a.el#b@3-4/function"
                                                  :file "lisp/a.el" :name "b" :beg 3 :end 4))))))
    (atlas-model-merge-batch state batch1)
    (should (equal (plist-get (atlas-model-get-symbol state "elisp:lisp/a.el#a@1-2/function") :name) "a"))
    (atlas-model-merge-batch state batch2)
    (should (null (atlas-model-get-symbol state "elisp:lisp/a.el#a@1-2/function")))
    (should (equal (plist-get (atlas-model-get-symbol state "elisp:lisp/a.el#b@3-4/function") :name) "b"))))
