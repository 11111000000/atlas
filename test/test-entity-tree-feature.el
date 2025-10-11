;;; test-entity-tree-feature.el --- ERT tests for by-feature view -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas-dump)
(require 'atlas-entity-tree)

(ert-deftest atlas-entity-tree-by-feature-renders ()
  "By-feature view lists features and their providers/requirees."
  (let* ((root default-directory)
         (edges (list
                 ;; feature:foo
                 (list :type 'provide :from "lisp/a.el" :to "feature:foo")
                 (list :type 'require :from "lisp/b.el" :to "feature:foo")
                 ;; feature:bar
                 (list :type 'provide :from "lisp/c.el" :to "feature:bar")
                 (list :type 'require :from "lisp/a.el" :to "feature:bar"))))
    ;; Import edges into in-memory model
    (atlas-import root (list (cons :edges edges)))
    (let ((buf (atlas-entity-tree root)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        ;; Ensure we're in the right mode
        (should (derived-mode-p 'atlas-entity-tree-mode))
        ;; Basic labels
        (should (string-match-p "Features" (buffer-string)))
        (should (string-match-p "feature:foo" (buffer-string)))
        (should (string-match-p "feature:bar" (buffer-string)))
        ;; Provided/required sections have expected files
        (should (string-match-p "Provided by" (buffer-string)))
        (should (string-match-p "Required by" (buffer-string)))
        (should (string-match-p "lisp/a.el" (buffer-string)))
        (should (string-match-p "lisp/b.el" (buffer-string)))
        (should (string-match-p "lisp/c.el" (buffer-string)))))))

(provide 'test-entity-tree-feature)

;;; test-entity-tree-feature.el ends here
