;;; test-entity-tree-actions.el --- ERT tests for tree actions -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas-dump)
(require 'atlas-entity-tree)

(defun atlas--test--ensure-file (abs content)
  "Return ABS without creating a file; tests rely on visiting non-existent files."
  (make-directory (file-name-directory abs) t)
  abs)

(ert-deftest atlas-entity-tree-open-and-peek-at-point ()
  "Open file at point and run a smoke peek."
  (let* ((root default-directory)
         (edges (list
                 (list :type 'provide :from "lisp/a.el" :to "feature:foo")
                 (list :type 'require :from "lisp/b.el" :to "feature:foo")))
         (abs-a (expand-file-name "lisp/a.el" root))
         (abs-b (expand-file-name "lisp/b.el" root)))
    ;; Prepare files and import edges
    (atlas--test--ensure-file abs-a "(provide 'foo)\n(defun a () 1)\n")
    (atlas--test--ensure-file abs-b "(require 'foo)\n(defun b () 2)\n")
    (atlas-import root (list (cons :edges edges)))
    (let ((buf (atlas-entity-tree root)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "lisp/a.el" nil t))
        (call-interactively #'atlas-entity-tree-open-at-point)))
    ;; Verify file buffer is visiting a.el
    (let ((fb (get-file-buffer abs-a)))
      (should (buffer-live-p fb)))
    ;; Peek smoke on b.el line
    (let ((buf (atlas-entity-tree root)))
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "lisp/b.el" nil t))
        (should (atlas-entity-tree-peek-at-point))))))

(provide 'test-entity-tree-actions)

;;; test-entity-tree-actions.el ends here
