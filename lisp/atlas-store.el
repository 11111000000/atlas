;;; atlas-store.el --- Persistence for Atlas store -*- lexical-binding: t; -*-

;;; Commentary:
;; Read/write versioned .sexp files under <root>/.context/atlas/v1/.
;; Gzip by file suffix when atlas-store-compressed is non-nil.
;; Stores:
;;  - files.sexp: list of file plists
;;  - symbols.sexp: list of symbol plists
;;  - edges.sexp: list of edge plists
;;  - summaries.sexp: list of summary plists (reserved)

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
            (print-level nil)))
        (prin1 obj (current-buffer))
        (insert "\n"))
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

(defun atlas-store-load (root name)
  "Load list from store file NAME under ROOT. Returns list or nil."
  (let ((path (atlas-store--file root name)))
    (or (atlas-store--read path) '())))

(defun atlas-store-save (root name list)
  "Save LIST to store file NAME under ROOT."
  (let ((path (atlas-store--file root name)))
    (atlas-store--write path list)))

(defun atlas-store-load-files (root) (atlas-store-load root "files"))
(defun atlas-store-load-symbols (root) (atlas-store-load root "symbols"))
(defun atlas-store-load-edges (root) (atlas-store-load root "edges"))
(defun atlas-store-load-summaries (root) (atlas-store-load root "summaries"))

(defun atlas-store-save-files (root files) (atlas-store-save root "files" files))
(defun atlas-store-save-symbols (root symbols) (atlas-store-save root "symbols" symbols))
(defun atlas-store-save-edges (root edges) (atlas-store-save root "edges" edges))
(defun atlas-store-save-summaries (root sums) (atlas-store-save root "summaries" sums))

(defun atlas-store--remove-symbols-for-file (symbols rel)
  "Return SYMBOLS list without entries whose :file equals REL."
  (seq-remove (lambda (pl) (equal (plist-get pl :file) rel)) symbols))

(defun atlas-store--remove-edges-for-file (edges rel)
  "Return EDGES list without entries bound to REL either in :from or :to."
  (seq-remove (lambda (pl)
                (or (equal (plist-get pl :from) rel)
                    (equal (plist-get pl :to) rel)))
              edges))

(defun atlas-store-save-batch (root batch)
  "Save BATCH keys :files :symbols :edges :summaries to store.
If BATCH contains :files, overwrite files.sexp with given list.
If BATCH contains :symbols or :edges, remove prior entries for the given :file (if provided),
then append new ones."
  (let* ((files (alist-get :files batch))
         (symbols (alist-get :symbols batch))
         (edges (alist-get :edges batch))
         (sums (alist-get :summaries batch))
         (rel (alist-get :file batch)))
    ;; Files: if provided, overwrite inventory
    (when files
      (atlas-store-save-files root files))
    ;; Symbols: remove old for rel, then append
    (when symbols
      (let* ((current (atlas-store-load-symbols root))
             (curr2 (if rel (atlas-store--remove-symbols-for-file current rel) current))
             (new (nconc curr2 (copy-sequence symbols))))
        (atlas-store-save-symbols root new)))
    ;; Edges: remove old bound to rel (both :from and :to), then append
    (when edges
      (let* ((current (atlas-store-load-edges root))
             (curr2 (if rel (atlas-store--remove-edges-for-file current rel) current))
             (new (nconc curr2 (copy-sequence edges))))
        (atlas-store-save-edges root new)))
    ;; Summaries: append (no per-file removal semantics yet)
    (when sums
      (let* ((current (atlas-store-load-summaries root))
             (new (nconc current (copy-sequence sums))))
        (atlas-store-save-summaries root new)))
    t))

(provide 'atlas-store)

;;; atlas-store.el ends here
