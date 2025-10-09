;;; atlas-graph.el --- Graph API over Atlas edges -*- lexical-binding: t; -*-

;;; Commentary:
;; Build small subgraphs by selector with depth/edge-type filters.

;;; Code:

(require 'cl-lib)
(require 'seq)
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
    (list :nodes (let (acc) (maphash (lambda (k _v) (push k acc)) visited) (nreverse acc))
          :edges (nreverse (seq-uniq edges-acc))))))

(provide 'atlas-graph)

;;; atlas-graph.el ends here
