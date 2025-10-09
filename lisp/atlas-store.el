;;; atlas-store.el --- Persistence for Atlas store -*- lexical-binding: t; -*-

;;; Commentary:
;; Read/write versioned .sexp files under <root>/.context/atlas/v1/.
;; Gzip by file suffix when atlas-store-compressed is non-nil.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function atlas-root-dir "atlas" (root))

(defun atlas-store--file (root name &optional gz)
  "Build path for NAME under ROOT. If GZ is non-nil, add .gz."
  (let* ((dir (atlas-root-dir root))
         (suffix (if (or gz (bound-and-true-p atlas-store-compressed)) ".sexp.gz" ".sexp")))
    (expand-file-name (concat name suffix) dir)))

(defun atlas-store--write (path obj)
  "Safely write OBJ to PATH as printed sexp."
  (let* ((tmp (concat path ".tmp")))
    (with-temp-file tmp
      (let ((print-length nil)
            (print-level nil))
        (prin1 obj (current-buffer))
        (insert "\n")))
    (when (file-exists-p path) (delete-file path))
    (rename-file tmp path t)
    path))

(defun atlas-store--read (path)
  "Read sexp from PATH. Return nil if missing."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun atlas-store-save-meta (root meta)
  "Save META alist to meta.sexp under ROOT."
  (let ((path (atlas-store--file root "meta")))
    (atlas-store--write path meta)))

(defun atlas-store-load-meta (root)
  "Load meta alist from meta.sexp under ROOT, or nil."
  (let ((path (atlas-store--file root "meta")))
    (atlas-store--read path)))

;; Placeholders for future segmented files
(defun atlas-store-save-batch (root batch)
  "Save BATCH alist keys :files :symbols :edges :summaries to store.
Currently a noop stub for future persistence."
  (ignore root batch)
  t)

(provide 'atlas-store)

;;; atlas-store.el ends here
