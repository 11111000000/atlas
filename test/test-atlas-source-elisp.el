;;; test-atlas-source-elisp.el --- ERT tests for elisp provider -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas)
(require 'atlas-index)
(require 'atlas-store)
(require 'atlas-source-elisp)

(defun atlas-test--write (root rel content)
  (let* ((abs (expand-file-name rel root)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs
      (insert content))
    abs))

(ert-deftest atlas-elisp-provider-scan-basic ()
  "Full index of tiny root: defun + require/provide yields symbol and two edges."
  (let* ((root (make-temp-file "atlas-root-" t)))
    (unwind-protect
        (progn
          ;; Files
          (atlas-test--write root "x.el" "(provide 'x)\n")
          (atlas-test--write root "a.el" "(require 'x)\n(defun foo () \"doc\" (message \"hi\"))\n")
          ;; Index
          (atlas-open root)
          (atlas-index root t)
          ;; Check symbols
          (let* ((syms (atlas-store-load-symbols root)))
            (should (seq-some (lambda (s)
                                (and (equal (plist-get s :file) "a.el")
                                     (equal (plist-get s :name) "foo")
                                     (eq (plist-get s :kind) 'function)))
                              syms)))
          ;; Check edges
          (let* ((edges (atlas-store-load-edges root))
                 (has (lambda (type from to)
                        (seq-some (lambda (e)
                                    (and (eq (plist-get e :type) type)
                                         (equal (plist-get e :from) from)
                                         (equal (plist-get e :to) to)))
                                  edges))))
            (should (funcall has 'require "a.el" "feature:x"))
            (should (funcall has 'provide "x.el" "feature:x"))))
      (ignore-errors (delete-directory root t)))))

(provide 'test-atlas-source-elisp)
