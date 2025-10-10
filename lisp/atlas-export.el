;;; atlas-export.el --- Graph and LLM exports for Atlas -*- lexical-binding: t; -*-

;;; Commentary:
;; Export small graphs to DOT/Mermaid and build language-agnostic JSON packs for LLMs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'atlas)
(require 'atlas-graph)
(require 'atlas-plan)
(require 'atlas-model)

(defun atlas-export--dot-escape (s)
  "Escape string S for DOT label."
  (replace-regexp-in-string "\"" "\\\"" (or s "")))

(defun atlas-export--dot (graph)
  "Render GRAPH alist to DOT string."
  (let* ((nodes (alist-get :nodes graph))
         (edges (alist-get :edges graph))
         (sb (get-buffer-create " *atlas-dot*")))
    (with-current-buffer sb
      (erase-buffer)
      (insert "digraph Atlas {\n  rankdir=LR;\n  node [shape=box];\n")
      (dolist (n nodes)
        (insert (format "  \"%s\";\n" (atlas-export--dot-escape (format "%s" n)))))
      (dolist (e edges)
        (let* ((from (plist-get e :from))
               (to (plist-get e :to))
               (type (plist-get e :type)))
          (when (and from to)
            (insert (format "  \"%s\" -> \"%s\" [label=\"%s\"];\n"
                            (atlas-export--dot-escape (format "%s" from))
                            (atlas-export--dot-escape (format "%s" to))
                            (atlas-export--dot-escape (format "%s" type)))))))
      (insert "}\n")
      (buffer-string))))

(defun atlas-export--mermaid (graph)
  "Render GRAPH alist to Mermaid flowchart string."
  (let* ((nodes (alist-get :nodes graph))
         (edges (alist-get :edges graph))
         (ids (make-hash-table :test #'equal))
         (symseq 0)
         (sym (lambda (key)
                (or (gethash key ids)
                    (let ((s (format "N%d" (cl-incf symseq))))
                      (puthash key s ids) s))))
         (sb (get-buffer-create " *atlas-mermaid*")))
    (with-current-buffer sb
      (erase-buffer)
      (insert "graph LR\n")
      (dolist (n nodes)
        (let* ((kid (funcall sym n))
               (label (replace-regexp-in-string "\"" "\\\"" (format "%s" n))))
          (insert (format "  %s[\"%s\"]\n" kid label))))
      (dolist (e edges)
        (let* ((from (plist-get e :from))
               (to (plist-get e :to))
               (type (plist-get e :type)))
          (when (and from to)
            (insert (format "  %s --> %s:::edge_%s\n"
                            (funcall sym from) (funcall sym to) type)))))
      (buffer-string))))

(cl-defun atlas-graph-export (root selector &key depth edge-types format path)
  "Export graph for SELECTOR at ROOT with DEPTH and EDGE-TYPES to PATH.
FORMAT is 'dot or 'mermaid. PATH is required."
  (unless path (user-error "atlas-graph-export: PATH is required"))
  (let* ((graph (atlas-graph root selector :depth (or depth 1) :edge-types edge-types))
         (fmt (or format 'dot))
         (txt (pcase fmt
                ('dot (atlas-export--dot graph))
                ('mermaid (atlas-export--mermaid graph))
                (_ (user-error "atlas-graph-export: unknown format %S" fmt)))))
    (with-temp-file path
      (insert txt))
    (atlas-log :info "graph-export: root=%s nodes=%d edges=%d format=%s path=%s"
               root (length (alist-get :nodes graph)) (length (alist-get :edges graph)) fmt path)
    t))

;;;###autoload
(defun atlas-graph-export-command (root selector path &optional format depth)
  "Interactive wrapper for atlas-graph-export."
  (interactive
   (list (read-directory-name "Atlas root: " nil nil t)
         (read-string "Selector (file REL, symbol id, feature:NAME, or space-separated list): ")
         (read-file-name "Output path: ")
         (intern (completing-read "Format: " '("dot" "mermaid") nil t nil nil "dot"))
         (read-number "Depth: " 1)))
  (let ((sel (if (string-match-p "[ \t]" selector)
                 (split-string selector "[ \t]+" t)
               selector)))
    (atlas-graph-export root sel :path path :format format :depth (or depth 1))))

(defun atlas-export--pair-range (cons)
  "Convert CONS range to JSON-ready vector [beg end]."
  (vector (or (car-safe cons) 0) (or (cdr-safe cons) 0)))

(cl-defun atlas-export-llm (root query &key k budget graph-depth path)
  "Export a language-agnostic JSON pack for LLM for QUERY at ROOT to PATH.
Includes top items, files, spans, and a small graph."
  (unless path (user-error "atlas-export-llm: PATH is required"))
  (let* ((plan (atlas-plan-context root query :k (or k 12) :budget budget :model nil))
         (state (or (atlas-state root) (atlas-open root)))
         (items (alist-get :items plan))
         (files (alist-get :files plan))
         (spans (alist-get :spans plan))
         (starts files)
         (graph (atlas-graph root starts :depth (or graph-depth 1)))
         (top
          (mapcar
           (lambda (it)
             (let* ((r (alist-get :item it))
                    (id (alist-get :id r))
                    (sym (and id (atlas-model-get-symbol state id))))
               `((id . ,id)
                 (name . ,(alist-get :name r))
                 (kind . ,(and sym (plist-get sym :kind)))
                 (file . ,(alist-get :file r))
                 (range . ,(atlas-export--pair-range (alist-get :range r)))
                 (sig . ,(alist-get :sig r))
                 (doc1 . ,(alist-get :doc1 r))
                 (score . ,(alist-get :score r)) )))
           items))
         (json
          `((query . ,query)
            (top . ,top)
            (files . ,files)
            (spans . ,(mapcar (lambda (sp) `((file . ,(plist-get sp :file))
                                             (beg . ,(plist-get sp :beg))
                                             (end . ,(plist-get sp :end))))
                              spans))
            (graph . ((nodes . ,(alist-get :nodes graph))
                      (edges . ,(mapcar (lambda (e)
                                          `((type . ,(plist-get e :type))
                                            (from . ,(plist-get e :from))
                                            (to . ,(plist-get e :to))))
                                        (alist-get :edges graph)))))
            (est_tokens . ,(alist-get :est-tokens plan))
            (rationale . ,(alist-get :rationale plan)))))
    (with-temp-file path
      (insert (json-serialize json :object-type 'alist)))
    (atlas-log :info "llm-export: root=%s items=%d files=%d nodes=%d edges=%d path=%s"
               root (length top) (length files)
               (length (alist-get :nodes graph)) (length (alist-get :edges graph)) path)
    t))

;;;###autoload
(defun atlas-export-llm-command (root query path &optional k budget graph-depth)
  "Interactive wrapper for atlas-export-llm."
  (interactive
   (list (read-directory-name "Atlas root: " nil nil t)
         (read-string "Query: ")
         (read-file-name "Output JSON path: ")
         (read-number "Top K: " 12)
         (read-number "Budget (tokens): " atlas-plan-default-budget)
         (read-number "Graph depth: " 1)))
  (atlas-export-llm root query :k k :budget budget :graph-depth graph-depth :path path))

(provide 'atlas-export)

;;; atlas-export.el ends here
