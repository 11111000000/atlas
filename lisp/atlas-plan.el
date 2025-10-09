;;; atlas-plan.el --- Context planning for LLM -*- lexical-binding: t; -*-

;;; Commentary:
;; Packs a simple plan from query results within a token budget.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'atlas)
(require 'atlas-query)

(cl-defun atlas-plan-context (root query &key k budget model)
  "Build context plan for QUERY at ROOT.
Return alist with :files :spans :docs :rationale :est-tokens :items."
  (let* ((model (or model atlas-plan-model))
         (budget (or budget atlas-plan-default-budget))
         (k (or k 12))
         (results (atlas-query root query :k k))
         ;; Placeholder packing: no spans yet
         (items (mapcar (lambda (r) (list :why "lexical match" :item r :tokens 64))
                        results))
         (est (* 64 (length items))))
    (list :files '()
          :spans '()
          :docs '()
          :rationale (format "Model=%s lexical plan under budget=%d" model budget)
          :est-tokens (min est budget)
          :items items)))

(provide 'atlas-plan)

;;; atlas-plan.el ends here
