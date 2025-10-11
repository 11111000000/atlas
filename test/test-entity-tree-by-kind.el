;;; test-entity-tree-by-kind.el --- ERT tests for by-kind view -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas)
(require 'atlas-dump)
(require 'atlas-entity-tree)

(ert-deftest atlas-entity-tree-by-kind-renders ()
  "By-kind view groups symbols by kind and lists their names."
  (let* ((root default-directory)
         ;; Build some symbols across two files
         (id-f (atlas--symbol-id :lang "elisp" :rel "lisp/a.el" :name "foo" :beg 1 :end 2 :kind "function"))
         (id-m (atlas--symbol-id :lang "elisp" :rel "lisp/a.el" :name "bar" :beg 3 :end 4 :kind "macro"))
         (id-v (atlas--symbol-id :lang "elisp" :rel "lisp/b.el" :name "x"   :beg 5 :end 6 :kind "var"))
         (batch-a (list (cons :file "lisp/a.el")
                        (cons :symbols
                              (list (list :id id-f :file "lisp/a.el" :name "foo" :kind 'function :beg 1 :end 2)
                                    (list :id id-m :file "lisp/a.el" :name "bar" :kind 'macro    :beg 3 :end 4)))))
         (batch-b (list (cons :file "lisp/b.el")
                        (cons :symbols
                              (list (list :id id-v :file "lisp/b.el" :name "x"   :kind 'var      :beg 5 :end 6))))))
    ;; Import batches into in-memory model
    (atlas-import root batch-a)
    (atlas-import root batch-b)
    (let ((buf (atlas-entity-tree root)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (atlas-entity-tree-set-view 'by-kind)
        (goto-char (point-min))
        (let ((txt (buffer-string)))
          (should (string-match-p "Symbols by kind" txt))
          (should (string-match-p "Function" txt))
          (should (string-match-p "Macro" txt))
          (should (string-match-p "Var" txt))
          (should (string-match-p "foo" txt))
          (should (string-match-p "bar" txt))
          (should (string-match-p "x" txt)))))))

(provide 'test-entity-tree-by-kind)

;;; test-entity-tree-by-kind.el ends here
