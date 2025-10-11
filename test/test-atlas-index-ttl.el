;;; test-atlas-index-ttl.el --- ERT tests for TTL/changed-only indexing -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas)
(require 'atlas-index)

(defun atlas-test--write-file (root rel content)
  "Write CONTENT to ROOT/REL, creating directories as needed."
  (let* ((abs (expand-file-name rel (file-name-as-directory root)))
         (dir (file-name-directory abs)))
    (make-directory dir t)
    (with-temp-file abs
      (insert content))
    abs))

(ert-deftest atlas-index-ttl-changed-only ()
  "When TTL has not expired, a changed-only run reports non-zero deltas for modified files.
We ensure mtime ticks to avoid FS resolution issues."
  (let* ((root (make-temp-file "atlas-root-" t))
         (atlas-index-ttl 3600.0)   ; long TTL → changed-only policy
         (atlas-hash-content t))    ; strengthen detection in case mtime granularity is coarse
    (unwind-protect
        (progn
          ;; Initial file and full index
          (atlas-test--write-file root "lisp/x.el" "(provide 'x)\n")
          (let ((res1 (atlas-index root t)))
            (should (> (plist-get res1 :symbols) 0)))
          ;; Ensure mtime advanced reliably on all filesystems
          (sleep-for 1.1)
          ;; Modify the same file with different content to guarantee detection (size/hash)
          (atlas-test--write-file root "lisp/x.el" "(provide 'x)\n;; touch\n")
          ;; Changed-only run (TTL not expired). Use explicit CHANGED list for determinism across filesystems.
          (let ((res2 (atlas-index root (list "lisp/x.el"))))
            ;; Accept symbols or edges delta (provider may only emit provide/require).
            (should (or (> (plist-get res2 :symbols) 0)
                        (> (plist-get res2 :edges) 0)))))
      (ignore-errors (delete-directory root t)))))

(provide 'test-atlas-index-ttl)
