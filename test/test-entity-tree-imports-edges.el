;;; test-entity-tree-imports-edges.el --- ERT tests for imports/edges views -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas-dump)
(require 'atlas-entity-tree)

(ert-deftest atlas-entity-tree-imports-view-renders ()
  "Imports view lists features with providers and requirees."
  (let* ((root default-directory)
         (edges (list
                 ;; feature:foo
                 (list :type 'provide :from "lisp/a.el" :to "feature:foo")
                 (list :type 'require :from "lisp/b.el" :to "feature:foo")
                 ;; feature:bar
                 (list :type 'provide :from "lisp/c.el" :to "feature:bar")
                 (list :type 'require :from "lisp/a.el" :to "feature:bar"))))
    (atlas-import root (list (cons :edges edges)))
    (let ((buf (atlas-entity-tree root)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (atlas-entity-tree-set-view 'imports)
        (let ((txt (buffer-string)))
          (should (string-match-p "Imports/Provides" txt))
          (should (string-match-p "feature:foo" txt))
          (should (string-match-p "feature:bar" txt))
          (should (string-match-p "Provided by" txt))
          (should (string-match-p "Required by" txt))
          (should (string-match-p "lisp/a\\.el" txt))
          (should (string-match-p "lisp/b\\.el" txt))
          (should (string-match-p "lisp/c\\.el" txt)))))))

(ert-deftest atlas-entity-tree-edges-view-renders-around-file ()
  "Edges view shows out edges for a file selector."
  (let* ((root default-directory)
         (edges (list
                 (list :type 'provide :from "lisp/a.el" :to "feature:foo")
                 (list :type 'require :from "lisp/b.el" :to "feature:foo"))))
    (atlas-import root (list (cons :edges edges)))
    (let ((buf (atlas-entity-tree-edges root "lisp/a.el" 1)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (let ((txt (buffer-string)))
          (should (string-match-p "Edges around: lisp/a\\.el" txt))
          (should (string-match-p "Out edges" txt))
          (should (string-match-p "provide -> feature:foo" txt)))))))

(ert-deftest atlas-entity-tree-edges-view-renders-around-feature ()
  "Edges view shows in edges for a feature selector."
  (let* ((root default-directory)
         (edges (list
                 (list :type 'provide :from "lisp/a.el" :to "feature:foo")
                 (list :type 'require :from "lisp/b.el" :to "feature:foo"))))
    (atlas-import root (list (cons :edges edges)))
    (let ((buf (atlas-entity-tree-edges root "feature:foo" 1)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (let ((txt (buffer-string)))
          (should (string-match-p "Edges around: feature:foo" txt))
          (should (string-match-p "In edges" txt))
          (should (string-match-p "provide <- lisp/a\\.el" txt))
          (should (string-match-p "require <- lisp/b\\.el" txt)))))))

(provide 'test-entity-tree-imports-edges)

;;; test-entity-tree-imports-edges.el ends here