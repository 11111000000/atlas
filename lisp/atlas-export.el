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

(defun atlas-export--apm-quote (s)
  "Quote string S for APM v2 lines."
  (concat "\""
          (replace-regexp-in-string "\"" "\\\\\"" (or s ""))
          "\""))

(defun atlas-export--apm-list (vals)
  "Render VALS as [v1, v2, ...] for APM. Strings without spaces are emitted as idents."
  (concat
   "["
   (mapconcat
    (lambda (v)
      (cond
       ((stringp v)
        ;; If it looks like an IDENT (no spaces/quotes/brackets/commas), print raw; else quote.
        (if (string-match-p "[ \t\n\\[\\]\",]" v)
            (atlas-export--apm-quote v)
          v))
       ((symbolp v) (symbol-name v))
       ((numberp v) (format "%s" v))
       ((eq v t) "t")
       ((null v) "f")
       (t (format "%s" v))))
    vals ", ")
   "]"))

(defun atlas-export--apm-render (lines path)
  "Write LINES to PATH, each item a string."
  (with-temp-file path
    (dolist (ln lines) (insert ln "\n"))))

(cl-defun atlas-export-apm-v2 (root &key sections budget path)
  "Export Atlas Prompt Map v2 for ROOT to PATH.
SECTIONS is a list among (header overview api edges entrypoints enrichment notes).
BUDGET is a soft cap (lines) for API and Edges; defaults reasonable if nil."
  (unless path (user-error "atlas-export-apm-v2: PATH is required"))
  (let* ((state (or (atlas-state root) (atlas-open root)))
         (meta  (plist-get state :meta))
         (counts (or (ignore-errors (atlas-store-counts root)) (plist-get meta :counts)))
         (langs (or (plist-get meta :languages) '(elisp)))
         (files (ignore-errors (atlas-store-load-files root)))
         (symbols (ignore-errors (atlas-store-load-symbols root)))
         (edges (ignore-errors (atlas-store-load-edges root)))
         (facts (or (ignore-errors (atlas-model-active-facts state))
                    (ignore-errors (atlas-store-load-facts root))))
         (summaries (ignore-errors (atlas-store-load-summaries root)))
         (sections (or sections '(header overview api edges entrypoints enrichment notes)))
         (api-cap (or budget 400))
         (edges-cap (or budget 800))
         (lines '()))
    ;; 1) Header
    (when (memq 'header sections)
      (let ((hdr (format "Project: root=%s languages=%s files=%s symbols=%s edges=%s facts=%s generated=%s"
                         (file-name-as-directory (expand-file-name (plist-get meta :project-root)))
                         (atlas-export--apm-list (mapcar (lambda (x) (format "%s" x)) langs))
                         (or (plist-get counts :files) 0)
                         (or (plist-get counts :symbols) 0)
                         (or (plist-get counts :edges) 0)
                         (or (plist-get counts :facts) 0)
                         (format "%.3f" (or (plist-get meta :generated-at) (float-time))))))
        (push hdr lines)))
    ;; Precompute provides/imports per file for overview
    (let* ((edges-sorted
            (seq-sort
             (lambda (a b)
               (let ((ta (format "%s" (plist-get a :type)))
                     (tb (format "%s" (plist-get b :type)))
                     (fa (format "%s" (plist-get a :from)))
                     (fb (format "%s" (plist-get b :from)))
                     (oa (format "%s" (plist-get a :to)))
                     (ob (format "%s" (plist-get b :to))))
                 (or (string-lessp ta tb)
                     (and (string= ta tb)
                          (or (string-lessp fa fb)
                              (and (string= fa fb) (string-lessp oa ob)))))))
             (or edges '()))))
      ;; 2) Overview
      (when (memq 'overview sections)
        (let ((files-sorted
               (seq-sort (lambda (a b)
                           (string-lessp (or (plist-get a :path) "")
                                         (or (plist-get b :path) "")))
                         (or files '()))))
          (dolist (f files-sorted)
            (let* ((rel (or (plist-get f :path) ""))
                   (lang (format "%s" (or (plist-get f :lang) "")))
                   (prov (seq-filter (lambda (e) (and (equal (plist-get e :from) rel)
                                                 (eq (plist-get e :type) 'provide)))
                                     edges-sorted))
                   (reqs (seq-filter (lambda (e) (and (equal (plist-get e :from) rel)
                                                 (eq (plist-get e :type) 'require)))
                                     edges-sorted))
                   (provides (mapcar (lambda (e) (format "%s" (plist-get e :to))) prov))
                   (imports  (mapcar (lambda (e) (format "%s" (plist-get e :to))) reqs))
                   (sumtxt (let ((s (seq-find (lambda (x) (equal (plist-get x :file) rel)) (or summaries '()))))
                             (and s (plist-get s :summary)))))
              (push (concat "Module: path=" rel
                            " lang=" lang
                            (when sumtxt (concat " summary=" (atlas-export--apm-quote (string-trim sumtxt))))
                            " provides=" (atlas-export--apm-list provides)
                            " imports=" (atlas-export--apm-list imports))
                    lines))))))
    ;; 3) API surface (simple: all symbols; cap by api-cap)
    (when (memq 'api sections)
      (let ((syms-sorted
             (seq-sort
              (lambda (a b)
                (let ((fa (or (plist-get a :file) ""))
                      (fb (or (plist-get b :file) ""))
                      (ba (or (plist-get a :beg) 0))
                      (bb (or (plist-get b :beg) 0))
                      (na (or (plist-get a :name) ""))
                      (nb (or (plist-get b :name) "")))
                  (or (string-lessp fa fb)
                      (and (string= fa fb)
                           (or (< ba bb)
                               (and (= ba bb) (string-lessp na nb)))))))
              (or symbols '()))))
        (let ((emitted 0))
          (dolist (s syms-sorted)
            (when (< emitted api-cap)
              (let* ((k (or (plist-get s :kind) 'symbol))
                     (nm (or (plist-get s :name) ""))
                     (rel (or (plist-get s :file) ""))
                     (beg (or (plist-get s :beg) 0))
                     (end (or (plist-get s :end) 0))
                     (sig (plist-get s :sig))
                     (doc1 (plist-get s :doc1))
                     (exp? (if (plist-get s :exported?) "t" "f")))
                (push (concat "API: kind=" (format "%s" k)
                              " name=" nm
                              " file=" rel
                              " range=" (format "%d..%d" beg end)
                              " exported?=" exp?
                              (when sig (concat " sig=" (atlas-export--apm-quote sig)))
                              (when doc1 (concat " doc=" (atlas-export--apm-quote doc1))))
                      lines)
                (setq emitted (1+ emitted))))))))
    ;; 4) Edges (sorted, cap)
    (when (memq 'edges sections)
      (let ((emitted 0))
        (dolist (e edges-sorted)
          (when (< emitted edges-cap)
            (let ((t (format "%s" (plist-get e :type)))
                  (from (format "%s" (plist-get e :from)))
                  (to (format "%s" (plist-get e :to))))
              (push (concat "Edge: type=" t " from=" from " to=" to) lines)
              (setq emitted (1+ emitted)))))))
    ;; 5) Entrypoints (from facts)
    (when (and (memq 'entrypoints sections) facts)
      (let ((ents (seq-filter (lambda (f) (eq (plist-get f :predicate) 'entrypoint)) facts)))
        (setq ents
              (seq-sort
               (lambda (a b)
                 (let ((sa (format "%s" (plist-get a :subject)))
                       (sb (format "%s" (plist-get b :subject))))
                   (string-lessp sa sb)))
               ents))
        (dolist (f ents)
          (let* ((subj (format "%s" (plist-get f :subject)))
                 (obj  (plist-get f :object))
                 (reason (cond
                          ((stringp obj) obj)
                          ((symbolp obj) (symbol-name obj))
                          (t (format "%s" obj)))))
            (push (concat "Entry: "
                          (if (and (string-match-p ":" subj) (string-match-p "#" subj))
                              (concat "symbol=" subj)
                            (concat "file=" subj))
                          " reason=" (atlas-export--apm-quote reason))
                  lines)))))
    ;; 6) Enrichment (facts)
    (when (and (memq 'enrichment sections) facts)
      (let ((facts-sorted
             (seq-sort
              (lambda (a b)
                (let ((sa (format "%s" (plist-get a :subject)))
                      (sb (format "%s" (plist-get b :subject)))
                      (pa (format "%s" (plist-get a :predicate)))
                      (pb (format "%s" (plist-get b :predicate)))
                      (oa (format "%s" (plist-get a :object)))
                      (ob (format "%s" (plist-get b :object))))
                  (or (string-lessp sa sb)
                      (and (string= sa sb)
                           (or (string-lessp pa pb)
                               (and (string= pa pb) (string-lessp oa ob)))))))
              facts)))
        (dolist (f facts-sorted)
          (let* ((subj (format "%s" (plist-get f :subject)))
                 (pred (format "%s" (plist-get f :predicate)))
                 (obj  (plist-get f :object))
                 (src  (format "%s" (plist-get f :source)))
                 (conf (or (plist-get f :confidence) 0.0))
                 (oval (cond
                        ((stringp obj) (atlas-export--apm-quote obj))
                        ((symbolp obj) (symbol-name obj))
                        (t (format "%s" obj)))))
            (push (format "Fact: subj=%s pred=%s obj=%s source=%s conf=%.2f"
                          subj pred oval src conf)
                  lines))))))
  ;; 7) Notes/TODO from summaries
  (when (and (memq 'notes sections) summaries)
    (dolist (s summaries)
      (let* ((rel (plist-get s :file))
             (txt (plist-get s :summary)))
        (when (and (stringp rel) (stringp txt))
          (let ((lls (split-string txt "\n" t)))
            (dolist (ln lls)
              (when (string-match "^[ \t]*\\(TODO\\|FIXME\\)[: -]+\\(.*\\S\\)" ln)
                (let ((tag (match-string 1 ln))
                      (body (string-trim (match-string 2 ln))))
                  (push (concat "Note: file=" rel
                                " tag=" tag
                                " text=" (atlas-export--apm-quote body))
                        lines)))))))))
  ;; Deterministic final ordering within sections preserved by push + reverse
  (setq lines (nreverse lines))
  (atlas-export--apm-render lines path)
  (atlas-log :info "apm-v2: root=%s lines=%d path=%s" root (length lines) path)
  t)

;;;###autoload
(defun atlas-export-apm-v2-command (root path &optional sections budget)
  "Interactive wrapper for atlas-export-apm-v2."
  (interactive
   (list (read-directory-name "Atlas root: " nil nil t)
         (read-file-name "Output APM path: ")
         nil
         (read-number "Soft line budget (applies to API/Edges): " 600)))
  (atlas-export-apm-v2 root :path path :sections sections :budget budget))

(cl-defun atlas-export-llm-pack (root query &key k budget graph-depth path)
  "Export an interoperable sexp LLM pack (primary) for QUERY at ROOT to PATH."
  (unless path (user-error "atlas-export-llm-pack: PATH is required"))
  (let* ((state (or (atlas-state root) (atlas-open root)))
         (k (or k 12))
         (budget (or budget atlas-plan-default-budget))
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
                    (sym (and id (atlas-model-get-symbol state id)))
                    (kval (or (and sym (plist-get sym :kind)) 'symbol)))
               `((id . ,id)
                 (name . ,name)
                 (kind . ,(if (symbolp kval) (symbol-name kval) kval))
                 (file . ,file)
                 (range . ,(vector beg end))
                 (sig . ,sig)
                 (doc1 . ,doc1)
                 (score . ,score))))
           (or results '())))
         ;; Deterministic order for TOP
         (top (seq-sort (lambda (a b) (string< (or (alist-get 'id a) "")
                                          (or (alist-get 'id b) "")))
                        top))
         (plan (ignore-errors (atlas-plan-context root query :k k :budget budget :model 'brief)))
         (plan-files (and plan (alist-get :files plan)))
         (plan-spans (and plan (alist-get :spans plan)))
         (est-tokens (or (and plan (alist-get :est-tokens plan)) 0))
         (rationale (or (and plan (alist-get :rationale plan)) "brief: lexical+1hop plan")))

    (let* ((starts (or plan-files
                       (seq-uniq (seq-filter #'identity (mapcar (lambda (o) (alist-get 'file o)) top)))))
           (graph (condition-case err
                      (atlas-graph root starts :depth (or graph-depth 1))
                    (error
                     (atlas-log :error "llm-pack: graph error: %S" err)
                     (list (cons :nodes '()) (cons :edges '())))))
           (sexp
            `(:query ,query
                     :top ,(vconcat top)
                     :files ,(vconcat (or starts '()))
                     :spans ,(vconcat (mapcar (lambda (sp)
                                                `((file . ,(plist-get sp :file))
                                                  (beg . ,(plist-get sp :beg))
                                                  (end . ,(plist-get sp :end))))
                                              (or plan-spans '())))
                     :graph (:nodes ,(vconcat (alist-get :nodes graph))
                                    :edges ,(vconcat (mapcar (lambda (e)
                                                               (let ((tval (plist-get e :type)))
                                                                 `((type . ,(if (symbolp tval) (symbol-name tval) tval))
                                                                   (from . ,(plist-get e :from))
                                                                   (to . ,(plist-get e :to)))))
                                                             (alist-get :edges graph))))
                     :est_tokens ,est-tokens
                     :rationale ,rationale)))
      (with-temp-file path
        (let ((print-length nil) (print-level nil))
          (prin1 sexp (current-buffer))
          (insert "\n")))
      (atlas-log :info "llm-pack(sexp): root=%s items=%d files=%d path=%s"
                 root (length top) (length starts) path)
      t)))

;;;###autoload
(defun atlas-export-llm-pack-command (root query path &optional k budget graph-depth)
  "Interactive wrapper for atlas-export-llm-pack."
  (interactive
   (list (read-directory-name "Atlas root: " nil nil t)
         (read-string "Query: ")
         (read-file-name "Output SEXP path: ")
         (read-number "Top K: " 12)
         (read-number "Budget (tokens): " atlas-plan-default-budget)
         (read-number "Graph depth: " 1)))
  (atlas-export-llm-pack root query :k k :budget budget :graph-depth graph-depth :path path))

(provide 'atlas-export)

;;; atlas-export.el ends here
