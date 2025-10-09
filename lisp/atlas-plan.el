;;; atlas-plan.el --- Context planning for LLM -*- lexical-binding: t; -*-

;;; Commentary:
;; Packs a plan from query results within a token budget.
;; Adds 1-hop expansion via require/provide.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'atlas)
(require 'atlas-query)
(require 'atlas-model)

(defun atlas-plan--file-span-around (abs-file beg end &optional lines)
  "Compute span around BEG..END in ABS-FILE, extend by LINES lines on both sides."
  (let ((lines (or lines 3)))
    (condition-case _
        (with-temp-buffer
          (insert-file-contents abs-file)
          (let* ((b (max 1 beg))
                 (e (min (point-max) (max end beg))))
            (save-excursion
              (goto-char b)
              (forward-line (- lines))
              (setq b (line-beginning-position)))
            (save-excursion
              (goto-char e)
              (forward-line lines)
              (setq e (line-end-position)))
            (list :beg b :end e)))
      (error (list :beg beg :end end)))))

(defun atlas-plan--estimate-tokens (span)
  "Roughly estimate tokens for SPAN by char length / 4."
  (let ((len (max 0 (- (plist-get span :end) (plist-get span :beg)))))
    (ceiling (/ (float len) 4.0))))

(cl-defun atlas-plan-context (root query &key k budget model)
  "Build context plan for QUERY at ROOT.
Return alist with :files :spans :docs :rationale :est-tokens :items."
  (let* ((state (or (atlas-state root) (atlas-open root)))
         (model (or model atlas-plan-model))
         (budget (or budget atlas-plan-default-budget))
         (k (or k 12))
         (results (atlas-query root query :k k))
         (spans '())
         (files (make-hash-table :test #'equal))
         (items '())
         (spent 0))
    ;; Primary items as spans around definitions
    (dolist (r results)
      (let* ((rel (alist-get :file r))
             (range (alist-get :range r))
             (beg (or (car-safe range) 0))
             (end (or (cdr-safe range) 0))
             (abs (expand-file-name rel (file-name-as-directory (plist-get state :root))))
             (se (atlas-plan--file-span-around abs beg end 3))
             (span2 (list :file rel :beg (plist-get se :beg) :end (plist-get se :end)))
             (tok (atlas-plan--estimate-tokens span2)))
        (when (< (+ spent tok) budget)
          (push span2 spans)
          (setq spent (+ spent tok))
          (puthash rel t files)
          (push (list :why "lexical match" :item r :tokens tok) items))))
    ;; 1-hop expansion: require/provide
    (dolist (r results)
      (let* ((rel (alist-get :file r))
             (edges (atlas-model-edges-out state rel)))
        (when edges
          (dolist (e edges)
            (when (and (eq (plist-get e :type) 'require)
                       (stringp (plist-get e :to)))
              (let* ((feature (plist-get e :to))
                     (prov (atlas-model-edges-in state feature)))
                (dolist (pe prov)
                  (when (eq (plist-get pe :type) 'provide)
                    (let ((frel (plist-get pe :from)))
                      (puthash frel t files))))))))))
    (list :files (let (acc) (maphash (lambda (k _v) (push k acc)) files) (nreverse acc))
          :spans (nreverse spans)
          :docs '()
          :rationale (format "Model=%s lexical+1hop plan under budget=%d" model budget)
          :est-tokens spent
          :items (nreverse items))))

(provide 'atlas-plan)

;;; atlas-plan.el ends here
