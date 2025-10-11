;;; test-entity-tree-plan.el --- ERT tests for plan view -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas-dump)
(require 'atlas-entity-tree)

(ert-deftest atlas-entity-tree-plan-view-renders ()
  "Plan view renders files and spans for a query and includes 1-hop expansion."
  (let* ((root default-directory)
         ;; Symbols: foo in a.el
         (sym-a (list :id "elisp:lisp/a.el#foo@10-20/function"
                      :file "lisp/a.el" :name "foo" :kind 'function :beg 10 :end 20))
         ;; Edges: a.el requires feature:foo, b.el provides feature:foo (1-hop expansion)
         (edges-a (list (list :type 'require :from "lisp/a.el" :to "feature:foo")))
         (edges-b (list (list :type 'provide :from "lisp/b.el" :to "feature:foo"))))
    ;; Import batches
    (atlas-import root (list (cons :file "lisp/a.el")
                             (cons :symbols (list sym-a))
                             (cons :edges edges-a)))
    (atlas-import root (list (cons :file "lisp/b.el")
                             (cons :symbols '())
                             (cons :edges edges-b)))
    ;; Open plan view
    (let ((buf (atlas-entity-tree-plan root "foo" 12 1000)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (should (derived-mode-p 'atlas-entity-tree-mode))
        (let ((txt (buffer-string)))
          (should (string-match-p "^Plan: foo" txt))
          (should (string-match-p "Files (2)" txt))
          (should (string-match-p "lisp/a\\.el" txt))
          (should (string-match-p "lisp/b\\.el" txt))
          (should (string-match-p "span 10\\.\\.20" txt)))))))

(provide 'test-entity-tree-plan)

;;; test-entity-tree-plan.el ends here
