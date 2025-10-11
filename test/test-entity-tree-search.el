;;; test-entity-tree-search.el --- ERT tests for search view -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas)
(require 'atlas-dump)
(require 'atlas-entity-tree)

(ert-deftest atlas-entity-tree-search-view-renders ()
  "Search view renders results and shows edge context."
  (let* ((root default-directory)
         ;; Two files, one has symbol 'foo and edges provide/require
         (sym-a (list :id "elisp:lisp/a.el#foo@1-2/function"
                      :file "lisp/a.el" :name "foo" :kind 'function :beg 1 :end 2 :sig "(defun foo ()))"))
         (sym-b (list :id "elisp:lisp/b.el#bar@3-4/function"
                      :file "lisp/b.el" :name "bar" :kind 'function :beg 3 :end 4 :sig "(defun bar ())"))
         (edges-a (list
                   (list :type 'provide :from "lisp/a.el" :to "feature:foo")
                   (list :type 'require :from "lisp/a.el" :to "feature:util"))))
    ;; Import per-file batches so model indexes symbols and edges
    (atlas-import root (list (cons :file "lisp/a.el")
                             (cons :symbols (list sym-a))
                             (cons :edges edges-a)))
    (atlas-import root (list (cons :file "lisp/b.el")
                             (cons :symbols (list sym-b))
                             (cons :edges '())))
    ;; Open search view for "foo"
    (let ((buf (atlas-entity-tree-search root "foo" 10)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (should (derived-mode-p 'atlas-entity-tree-mode))
        (should (string-match-p "Search: foo" (buffer-string)))
        (should (string-match-p "lisp/a\\.el" (buffer-string)))
        ;; Out edges should include both provide/require targets
        (should (string-match-p "Out edges" (buffer-string)))
        (should (string-match-p "feature:foo" (buffer-string)))
        (should (string-match-p "feature:util" (buffer-string)))))))

(provide 'test-entity-tree-search)

;;; test-entity-tree-search.el ends here
