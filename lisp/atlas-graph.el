;;; atlas-graph.el --- Graph API over Atlas edges -*- lexical-binding: t; -*-

;;; Commentary:
;; Build small subgraphs by selector with depth/edge-type filters.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'atlas-log)
(require 'atlas)
(require 'atlas-model)

(defun atlas-graph--normalize-selector (sel)
  "Normalize SEL to a list of starting keys (file REL, feature:NAME or symbol id)."
  (cond
   ((null sel) '())
   ((stringp sel) (list sel))
   ((listp sel) (seq-filter #'identity (mapcar (lambda (x) (and (stringp x) x)) sel)))
   (t '())))

(cl-defun atlas-graph (root selector &key depth edge-types)
  "Return subgraph for SELECTOR at ROOT with DEPTH and EDGE-TYPES filter.
Returns alist (:nodes LIST :edges LIST). SELECTOR may be file REL, symbol id, or \"feature:...\"."
  (let* ((state (or (atlas-state root) (atlas-open root)))
         (starts (atlas-graph--normalize-selector selector))
         (depth (or depth 1))
         (edge-types (and edge-types (cl-remove-duplicates edge-types)))
         (visited (make-hash-table :test #'equal))
         (edges-acc '())
         (push-edge
          (lambda (e)
            (when (or (null edge-types) (member (plist-get e :type) edge-types))
              (push e edges-acc)))))
    (atlas-log :info "graph: root=%s starts=%d depth=%d edge-types=%S"
               root (length starts) depth edge-types)
    (cl-labels ((step (front d)
                  (when (and front (>= d 0))
                    (let ((next '()))
                      (dolist (k front)
                        (puthash k t visited)
                        ;; Out edges
                        (dolist (e (or (atlas-model-edges-out state k) '()))
                          (funcall push-edge e)
                          (let ((to (plist-get e :to)))
                            (when (and to (not (gethash to visited)))
                              (push to next))))
                        ;; In edges
                        (dolist (e (or (atlas-model-edges-in state k) '()))
                          (funcall push-edge e)
                          (let ((from (plist-get e :from)))
                            (when (and from (not (gethash from visited)))
                              (push from next)))))
                      (step (seq-uniq next) (1- d))))))
      (step starts depth))
    (let* ((nodes (let (acc) (maphash (lambda (k _v) (push k acc)) visited) (nreverse acc)))
           (edges (nreverse (seq-uniq edges-acc))))
      ;; Ensure starts appear even if no edges were traversed
      (when (and (null nodes) starts)
        (setq nodes (copy-sequence starts)))
      ;; Deterministic ordering for stable outputs
      (setq nodes (seq-sort #'string< nodes))
      (setq edges
            (seq-sort
             (lambda (a b)
               (let* ((ta (format "%s" (plist-get a :type)))
                      (tb (format "%s" (plist-get b :type)))
                      (fa (format "%s" (plist-get a :from)))
                      (fb (format "%s" (plist-get b :from)))
                      (oa (format "%s" (plist-get a :to)))
                      (ob (format "%s" (plist-get b :to))))
                 (or (string-lessp ta tb)
                     (and (string= ta tb)
                          (or (string-lessp fa fb)
                              (and (string= fa fb)
                                   (string-lessp oa ob)))))))
             edges))
      (atlas-log :debug "graph: result nodes=%d edges=%d" (length nodes) (length edges))
      (list (cons :nodes nodes) (cons :edges edges)))))

(provide 'atlas-graph)

;;; atlas-graph.el ends here
