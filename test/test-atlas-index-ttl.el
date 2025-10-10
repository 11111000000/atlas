;;; test-atlas-index-ttl.el --- ERT tests for TTL/changed-only -*- lexical-binding: t; -*-

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

(ert-deftest atlas-index-ttl-changed-only ()
  "With fresh index and long TTL: second run yields no changes; after touching a file, symbols>0."
  (let* ((root (make-temp-file "atlas-root-" t))
         (atlas-index-ttl 3600.0)) ;; large TTL to force changed-only path
    (unwind-protect
        (progn
          ;; Files
          (atlas-test--write root "x.el" "(provide 'x)\n")
          (atlas-test--write root "a.el" "(require 'x)\n(defun foo () (message \"hi\"))\n")
          ;; Full index
          (atlas-open root)
          (atlas-index root t)
          ;; No changes run
          (let* ((res (atlas-index root nil)))
            (should (equal (plist-get res :files) 0))
            (should (equal (plist-get res :symbols) 0))
            (should (equal (plist-get res :edges) 0)))
          ;; Modify a.el to trigger changed-only
          (sleep-for 1.1) ;; ensure mtime changes on coarse filesystems
          (with-temp-buffer
            (insert-file-contents (expand-file-name "a.el" root))
            (goto-char (point-max))
            (insert "\n;; touch\n")
            (write-region (point-min) (point-max) (expand-file-name "a.el" root) nil 'silent))
          (let* ((res2 (atlas-index root nil)))
            (should (> (plist-get res2 :symbols) 0))))
      (ignore-errors (delete-directory root t)))))

(provide 'test-atlas-index-ttl)
