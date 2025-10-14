;;; mk-index.el --- Generate spec-index.sexp and link-map.sexp -*- lexical-binding: t; -*-

;; Usage examples:
;;   emacs -Q --batch -l mk-index.el --eval="(atlas-spec-generate nil)"
;;   emacs -Q --batch -l mk-index.el --eval="(atlas-spec-generate \"spec/v1/\")"
;;   emacs -Q --batch -l mk-index.el --eval="(atlas-spec-generate \"s/v/\")"
;;
;; If ROOT is nil or does not exist, the generator will try common locations:
;;   ./spec/v1/ then ./s/v/ (first existing wins).

(require 'cl-lib)
(require 'seq)

(defun atlas--org-props (file)
  "Return top-level file PROPERTIES from FILE as an alist without visiting buffers.
Parses:
  - PROPERTIES drawer (:ID:, :STATUS:, :DEPENDS:, …)
  - #+TITLE and #+LANGUAGE keywords.
All keys are returned as lowercased keyword symbols (e.g., :id, :status)."
  (let ((case-fold-search t)
        (alist '()))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      ;; Parse #+TITLE and #+LANGUAGE
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+\\(TITLE\\|LANGUAGE\\):[ \t]*\\(.*\\)$" nil t)
          (let* ((key (downcase (match-string 1)))
                 (val (string-trim (match-string 2)))
                 (sym (intern (concat ":" key))))
            (push (cons sym val) alist))))
      ;; Parse PROPERTIES drawer
      (when (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
        (let ((start (point)))
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
            (let ((drawer (buffer-substring-no-properties start (match-beginning 0))))
              (with-temp-buffer
                (insert drawer)
                (goto-char (point-min))
                (while (re-search-forward "^[ \t]*:\\([^: \t]+\\):[ \t]*\\(.*\\)$" nil t)
                  (let* ((k (downcase (match-string 1)))
                         (v (string-trim (match-string 2)))
                         (sym (intern (concat ":" k))))
                    (push (cons sym v) alist)))))))))
    (nreverse alist)))

(defun atlas-spec--doc-entry (root file)
  "Build :docs entry plist for FILE under ROOT."
  (let* ((props (atlas--org-props file))
         (id (intern (or (cdr (assq :id props)) (file-name-base file))))
         (status (intern (or (cdr (assq :status props)) "Informative")))
         (depends (let ((d (cdr (assq :depends props))))
                    (if (and d (> (length d) 0))
                        (mapcar #'intern (split-string d "[ ,]+" t))
                      '()))))
    ;; Use file-relative-name for robustness instead of string-remove-prefix
    (list :id id
          :file (file-relative-name file root)
          :status status
          :depends depends)))

(defun atlas-spec--resolve-root (root)
  "Return an existing ROOT directory for specs.
Try ROOT (if non-nil), then ./spec/v1/, then ./s/v/."
  (let* ((cwd (or default-directory (expand-file-name ".")))
         (candidates (delq nil
                           (list (and root (expand-file-name root))
                                 (expand-file-name "spec/v1/" cwd)
                                 (expand-file-name "s/v/" cwd)))))
    (or (cl-find-if #'file-directory-p candidates)
        (error "No spec root directory found among: %S" candidates))))

(defun atlas-spec-generate (root)
  "Generate spec-index.sexp and a minimal link-map.sexp under ROOT directory.
ROOT may be nil. When nil or nonexistent, common locations are tried."
  (let* ((root (file-name-as-directory (atlas-spec--resolve-root root)))
         (orgs (directory-files-recursively root "\\.org\\'"))
         (docs (mapcar (lambda (f) (atlas-spec--doc-entry root f))
                       (seq-sort #'string< orgs)))
         (index-file (expand-file-name "spec-index.sexp" root))
         (link-file (expand-file-name "link-map.sexp" root)))
    ;; spec-index.sexp
    (with-temp-file index-file
      (let ((print-length nil) (print-level nil))
        (prin1 (list :docs docs) (current-buffer))
        (insert "\n")))
    ;; link-map.sexp — minimal: map file-level :ID to (file . :ID)
    (with-temp-file link-file
      (insert ";; Anchor → (file . CUSTOM_ID)\n(")
      (dolist (f orgs)
        (let* ((props (atlas--org-props f))
               (cid (intern (or (cdr (assq :id props))
                                (file-name-base f)))))
          (insert (format "\n (%s . (\"%s\" . %s))"
                          cid
                          (file-relative-name f root)
                          cid))))
      (insert "\n)\n"))
    (message "Generated: %s and %s" index-file link-file)
    (cons index-file link-file)))

(provide 'mk-index)
;;; mk-index.el ends here
