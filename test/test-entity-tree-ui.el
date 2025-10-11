;;; test-entity-tree-ui.el --- ERT UI smoke tests for entity tree -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas-dump)
(require 'atlas-entity-tree)

(defun atlas--test--ensure-file (abs content)
  "Ensure ABS exists with CONTENT."
  (make-directory (file-name-directory abs) t)
  (with-temp-file abs (insert content))
  abs)

(ert-deftest atlas-entity-tree-follow-mode-peek-smoke ()
  "Enable follow-mode and ensure auto-peek does not error."
  (let* ((root default-directory)
         (edges (list
                 (list :type 'provide :from "lisp/a.el" :to "feature:foo")
                 (list :type 'require :from "lisp/b.el" :to "feature:foo")))
         (abs-a (expand-file-name "lisp/a.el" root))
         (abs-b (expand-file-name "lisp/b.el" root)))
    (atlas--test--ensure-file abs-a "(provide 'foo)\n(defun a () 1)\n")
    (atlas--test--ensure-file abs-b "(require 'foo)\n(defun b () 2)\n")
    (atlas-import root (list (cons :edges edges)))
    (let ((buf (atlas-entity-tree root)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "lisp/a.el" nil t))
        (atlas-entity-tree-follow-mode 1)
        (should (atlas-entity-tree--follow-post-command))
        (atlas-entity-tree-follow-mode -1)))))

(ert-deftest atlas-entity-tree-actions-copy-path ()
  "Copy path action copies atlas-rel at point."
  (let* ((root default-directory)
         (edges (list (list :type 'provide :from "lisp/a.el" :to "feature:foo")))
         (abs-a (expand-file-name "lisp/a.el" root)))
    (atlas--test--ensure-file abs-a "(provide 'foo)\n")
    (atlas-import root (list (cons :edges edges)))
    (let ((buf (atlas-entity-tree root)))
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "lisp/a.el" nil t))
        (let ((copied (atlas-entity-tree-copy-at-point)))
          (should (string= copied "lisp/a.el"))
          (should (string= (current-kill 0 t) "lisp/a.el")))))))

(provide 'test-entity-tree-ui)

;;; test-entity-tree-ui.el ends here
