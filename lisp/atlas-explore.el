;;; atlas-explore.el --- Minimal explorer and Context export -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple buffer explorer for queries and helper to export a context group.

;;; Code:

(require 'cl-lib)
(require 'atlas)
(require 'atlas-query)
(require 'atlas-plan)

(defvar atlas-explore-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'atlas-explore-open-at-point)
    (define-key map (kbd "o")   #'atlas-explore-open-at-point)
    map)
  "Keymap for `atlas-explore-mode'.")

;;;###autoload
(define-derived-mode atlas-explore-mode special-mode "Atlas-Explore"
  "Minor explorer for Atlas results.
Keys:
  RET/o  open item at point (file at position)
  q      quit window (from `special-mode')."
  :group 'atlas)

(defun atlas-explore--open (root rel beg)
  "Open REL (relative to ROOT) and go to BEG."
  (let* ((abs (expand-file-name rel (file-name-as-directory root))))
    (unless (file-exists-p abs)
      (user-error "File does not exist: %s" abs))
    (find-file abs)
    (goto-char (or beg 1))
    (recenter)))

;;;###autoload
(defun atlas-explore-open-at-point ()
  "Open item at point using text properties set by explorer."
  (interactive)
  (let* ((rel (get-text-property (point) 'atlas-rel))
         (beg (get-text-property (point) 'atlas-beg))
         (root (get-text-property (point) 'atlas-root)))
    (if (and root rel)
        (atlas-explore--open root rel beg)
      (message "No item at point"))))

;;;###autoload
(defun atlas-explore (root query &optional k)
  "Explore Atlas ROOT for QUERY and show results in a temp buffer."
  (interactive (list (read-directory-name "Atlas root: " nil nil t)
                     (read-string "Query: ")
                     (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (let* ((k (or k 20))
         (results (atlas-query root query :k k))
         (buf (get-buffer-create "*Atlas Explore*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Atlas Explore — root: %s, query: %s, k=%d\n\n" root query k))
      (dolist (r results)
        (let* ((name (or (alist-get :name r) (alist-get :id r)))
               (score (alist-get :score r))
               (rel (alist-get :file r))
               (range (alist-get :range r))
               (beg (or (car-safe range) 1))
               (sig (or (alist-get :sig r) ""))
               (doc1 (or (alist-get :doc1 r) "")))
          (let ((line-start (point)))
            (insert (format "- %s  score=%s  " name score))
            (let ((btn-start (point)))
              (insert-text-button "[Open]"
                                  'help-echo "Open file at position"
                                  'follow-link t
                                  'action (lambda (_btn)
                                            (atlas-explore--open root rel beg))))
            (insert "\n")
            (add-text-properties
             line-start (point)
             (list 'atlas-root root 'atlas-rel rel 'atlas-beg beg 'mouse-face 'highlight))
            (insert (format "  file=%s  range=%S\n  sig=%s\n  doc1=%s\n\n"
                            rel range sig doc1)))))
      (atlas-explore-mode))
    (pop-to-buffer buf)))

(cl-defun atlas-build-context-group (root query &key k budget model)
  "Build a context group alist from QUERY using atlas-plan-context."
  (let* ((plan (atlas-plan-context root query :k (or k 12) :budget budget :model model)))
    (list :files (alist-get :files plan)
          :spans (alist-get :spans plan)
          :rationale (alist-get :rationale plan)
          :est-tokens (alist-get :est-tokens plan))))

(cl-defun atlas-export-to-context (group)
  "Export GROUP to Context Navigator. Placeholder returning GROUP."
  ;; Integrate with Navigator here when available.
  group)

(provide 'atlas-explore)

;;; atlas-explore.el ends here
