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
(require 'atlas-store)

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

(defun atlas-export--alist-indexed (lst)
  "Convert LST to an alist object with string indexes: ((\"0\" . v0) (\"1\" . v1) ...)."
  (let ((i 0) (acc nil))
    (dolist (x (or lst '()))
      (push (cons (number-to-string i) x) acc)
      (setq i (1+ i)))
    (nreverse acc)))

(cl-defun atlas-export-llm (root query &key k budget graph-depth path)
  "Export a language-agnostic JSON pack for LLM for QUERY at ROOT to PATH.
Builds top via atlas-query and derives files/spans/tokens via atlas-plan-context for determinism."
  (unless path (user-error "atlas-export-llm: PATH is required"))
  (let* ((state (or (atlas-state root) (atlas-open root)))
         (k (or k 12))
         (budget (or budget atlas-plan-default-budget))
         ;; Results via query (deterministic sorting by id as tiebreaker)
         (results (ignore-errors (atlas-query root query :k k)))
         (top
          (mapcar
           (lambda (r)
             (let* ((id (alist-get :id r))
                    (file (alist-get :file r))
                    (range (alist-get :range r))
                    (beg (or (car-safe range) 0))
                    (end (or (cdr-safe range) 0))
                    (name (alist-get :name r))
                    (sig (alist-get :sig r))
                    (doc1 (alist-get :doc1 r))
                    (score (or (alist-get :score r) 0))
                    ;; Try to preserve kind from in-memory symbol if available
                    (sym (and id (atlas-model-get-symbol state id)))
                    (kval (or (and sym (plist-get sym :kind)) 'symbol)))
               `((id . ,id)
                 (name . ,name)
                 (kind . ,(if (symbolp kval) (symbol-name kval) kval))
                 (file . ,file)
                 (range . ,(atlas-export--pair-range (cons beg end)))
                 (sig . ,sig)
                 (doc1 . ,doc1)
                 (score . ,score))))
           (or results '())))
         ;; Deterministic order for TOP
         (top (seq-sort (lambda (a b) (string< (or (alist-get 'id a) "")
                                          (or (alist-get 'id b) "")))
                        top))
         ;; Plan provides files/spans/tokens/rationale (already uses query internally)
         (plan (ignore-errors (atlas-plan-context root query :k k :budget budget :model 'brief)))
         (plan-files (and plan (alist-get :files plan)))
         (plan-spans (and plan (alist-get :spans plan)))
         (est-tokens (or (and plan (alist-get :est-tokens plan)) 0))
         (rationale (or (and plan (alist-get :rationale plan)) "brief: lexical+1hop plan"))
         ;; Files: prefer plan files, else derive from TOP; keep sorted
         (files (seq-sort #'string<
                          (seq-uniq
                           (or plan-files
                               (seq-filter #'identity (mapcar (lambda (o) (alist-get 'file o)) top))))))
         (starts files)
         (graph (condition-case err
                    (atlas-graph root starts :depth (or graph-depth 1))
                  (error
                   (atlas-log :error "llm-export: graph error: %S" err)
                   (list (cons :nodes '()) (cons :edges '())))))
         (edges (alist-get :edges graph))
         (imports (seq-sort #'string<
                            (seq-uniq
                             (seq-filter (lambda (x) (and (stringp x) (string-prefix-p "feature:" x)))
                                         (mapcar (lambda (e) (plist-get e :to)) edges))))))
    (let ((json
           `((graph . ((nodes . ,(vconcat (alist-get :nodes graph)))
                       (edges . ,(vconcat (mapcar (lambda (e)
                                                    (let ((tval (plist-get e :type)))
                                                      `((type . ,(if (symbolp tval) (symbol-name tval) tval))
                                                        (from . ,(plist-get e :from))
                                                        (to . ,(plist-get e :to)))))
                                                  edges)))))
             (query . ,query)
             (top . ,(vconcat top))
             (files . ,(vconcat files))
             (imports . ,(vconcat imports))
             (spans . ,(vconcat (mapcar (lambda (sp) `((file . ,(plist-get sp :file))
                                                  (beg . ,(plist-get sp :beg))
                                                  (end . ,(plist-get sp :end))))
                                        (or plan-spans '()))))
             (est_tokens . ,est-tokens)
             (rationale . ,rationale))))
      (with-temp-file path
        (insert (json-serialize json)))
      (atlas-log :info "llm-export: root=%s items=%d files=%d nodes=%d edges=%d path=%s"
                 root (length top) (length files)
                 (length (alist-get :nodes graph)) (length (alist-get :edges graph)) path)
      t)))

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
