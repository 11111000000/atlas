;;; atlas-entity-tree.el --- Entity-centric tree UI for Atlas -*- lexical-binding: t; -*-

;;; Commentary:
;; Skeleton of entity tree visualization: buffer/mode, basic render, event subscriptions,
;; soft/hard refresh. Views will be added in next steps.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'atlas)
(require 'atlas-log)
(require 'atlas-events)
(require 'atlas-model)
(require 'atlas-query)
(require 'atlas-graph)
(require 'atlas-plan)

;;;###autoload
(defgroup atlas-entity-tree nil
  "Entity-centric tree visualization for Atlas."
  :group 'atlas
  :prefix "atlas-entity-tree-")

(defcustom atlas-entity-tree-default-view 'by-feature
  "Default entity tree view. Placeholder until views are implemented."
  :type '(choice (const by-feature) (const by-kind) (const search) (symbol))
  :group 'atlas-entity-tree)

(defcustom atlas-entity-tree-section-style (if (featurep 'magit-section) 'magit-section 'text)
  "Renderer backend: 'magit-section or 'text (fallback)."
  :type '(choice (const magit-section) (const text))
  :group 'atlas-entity-tree)

(defcustom atlas-entity-tree-max-children 200
  "Max children rendered before pagination is required (future use)."
  :type 'integer
  :group 'atlas-entity-tree)

(defcustom atlas-entity-tree-icons t
  "If non-nil, show icons for features/files/symbol kinds when available (all-the-icons optional)."
  :type 'boolean
  :group 'atlas-entity-tree)

;; Optional icons support
(defvar atlas-entity-tree--icons-loaded (require 'all-the-icons nil 'noerror)
  "Non-nil when all-the-icons is available.")

(defun atlas-entity-tree--icon (type &optional arg)
  "Return icon string for TYPE and ARG if =atlas-entity-tree-icons' is non-nil.
TYPE is one of 'feature, 'file, or 'kind (ARG = kind symbol)."
  (when atlas-entity-tree-icons
    (let ((ai (featurep 'all-the-icons)))
      (pcase type
        ('feature
         (if ai
             (or (and (fboundp 'all-the-icons-octicon)
                      (all-the-icons-octicon "package" :v-adjust 0.0))
                 (and (fboundp 'all-the-icons-material)
                      (all-the-icons-material "extension" :v-adjust 0.0))
                 "[F]")
           "[F]"))
        ('file
         (if ai
             (let* ((name (and (stringp arg) (file-name-nondirectory arg)))
                    (ico (and (fboundp 'all-the-icons-icon-for-file)
                              (all-the-icons-icon-for-file (or name "")))))
               (or ico
                   (and (fboundp 'all-the-icons-material)
                        (all-the-icons-material "insert_drive_file" :v-adjust 0.0))
                   "[file]"))
           "[file]"))
        ('kind
         (let* ((k (if (symbolp arg) arg (and (stringp arg) (intern-soft arg)))))
           (cond
            (ai
             (pcase k
               ('function (and (fboundp 'all-the-icons-material)
                               (all-the-icons-material "functions" :v-adjust 0.0)))
               ('macro    (and (fboundp 'all-the-icons-octicon)
                               (all-the-icons-octicon "zap" :v-adjust 0.0)))
               ('var      (and (fboundp 'all-the-icons-octicon)
                               (all-the-icons-octicon "gear" :v-adjust 0.0)))
               ('custom   (and (fboundp 'all-the-icons-material)
                               (all-the-icons-material "settings" :v-adjust 0.0)))
               ('const    (and (fboundp 'all-the-icons-material)
                               (all-the-icons-material "lock" :v-adjust 0.0)))
               (_         (and (fboundp 'all-the-icons-faicon)
                               (all-the-icons-faicon "tag" :v-adjust 0.0)))))
            (t
             (pcase k
               ('function "ƒ")
               ('macro    "μ")
               ('var      "𝑣")
               ('custom   "☰")
               ('const    "○")
               (_         "*"))))))
        (_ "")))))

;; Docstring expand/collapse
(defcustom atlas-entity-tree-doc-max-lines 12
  "Max number of docstring lines to show when expanded."
  :type 'integer
  :group 'atlas-entity-tree)

(defcustom atlas-entity-tree-doc-ellipsis "…"
  "Ellipsis marker appended when docstring is truncated."
  :type 'string
  :group 'atlas-entity-tree)

(defvar-local atlas-entity-tree--doc-cache (make-hash-table :test #'equal)
  "Cache for docstrings keyed by (REL . BEG) in the tree buffer.")

(defcustom atlas-entity-tree-auto-refresh t
  "If non-nil, auto-refresh tree on :atlas-index-done events."
  :type 'boolean
  :group 'atlas-entity-tree)

(defcustom atlas-entity-tree-peek-lines 5
  "How many context lines to show in peek preview."
  :type 'integer
  :group 'atlas-entity-tree)

(defcustom atlas-entity-tree-peek-window-height 8
  "Preferred height of peek side window (in lines)."
  :type 'integer
  :group 'atlas-entity-tree)

(defcustom atlas-entity-tree-graph-depth 1
  "Default depth for edges view graph expansion."
  :type 'integer
  :group 'atlas-entity-tree)

(defvar atlas-entity-tree--buffer-name "*Atlas Entities*"
  "Default buffer name for the entity tree.")

(defvar-local atlas-entity-tree--root nil
  "Atlas root directory bound to this tree buffer (buffer-local).")

(defvar atlas-entity-tree--buffers nil
  "List of live buffers participating in entity tree updates.")

(defvar atlas-entity-tree--unsubs nil
  "Unsubscribe lambdas for Atlas event handlers.")

(defvar-local atlas-entity-tree--status ""
  "Status line shown at the top of the tree buffer (buffer-local).")

(defvar-local atlas-entity-tree--view nil
  "Current view symbol for this tree buffer (buffer-local).
One of: 'by-feature (implemented), 'by-kind, 'search (TBD).")

(defvar-local atlas-entity-tree--edges-selector nil
  "Current selector for 'edges view (file REL, feature:NAME, or symbol id).")

;;;###autoload
(defun atlas-entity-tree-set-view (view)
  "Set current VIEW for the tree buffer and refresh. VIEW is a symbol."
  (interactive
   (list (intern (completing-read "Entity tree view: "
                                  '("by-feature" "by-kind" "search" "imports" "edges" "plan")
                                  nil t nil nil
                                  (symbol-name (or atlas-entity-tree--view atlas-entity-tree-default-view))))))
  (unless (derived-mode-p 'atlas-entity-tree-mode)
    (user-error "Not in atlas-entity-tree buffer"))
  (setq-local atlas-entity-tree--view view)
  (atlas-entity-tree-refresh))

(defun atlas-entity-tree--state (root)
  "Return Atlas state for ROOT, opening if needed."
  (or (atlas-state root) (atlas-open root)))

(defun atlas-entity-tree--collect-features (state)
  "Collect list of feature keys (\"feature:NAME\") from STATE edges-in."
  (let* ((idxs (plist-get state :indexes))
         (ein (and idxs (plist-get idxs atlas-model--k-edges-in)))
         (acc '()))
    (when (hash-table-p ein)
      (maphash
       (lambda (k _v)
         (when (and (stringp k) (string-prefix-p "feature:" k))
           (push k acc)))
       ein))
    (seq-sort #'string< (seq-uniq acc))))

(defun atlas-entity-tree--files-for-feature (state feature type)
  "Return sorted list of file rels for FEATURE by edge TYPE ('provide or 'require) from STATE."
  (let* ((edges (atlas-model-edges-in state feature))
         (rels (seq-map (lambda (e) (plist-get e :from))
                        (seq-filter (lambda (e) (eq (plist-get e :type) type)) edges))))
    (seq-sort #'string< (seq-uniq (seq-filter #'stringp rels)))))

(defun atlas-entity-tree--insert-by-feature (root)
  "Insert textual tree for 'by-feature view for ROOT."
  (let* ((state (atlas-entity-tree--state root))
         (features (atlas-entity-tree--collect-features state)))
    (cl-labels
        ((atlas-entity-tree--print-file-and-symbols
           (rel)
           ;; File line (clickable)
           (let ((lstart (point)))
             (insert (format "    %s %s\n" (or (atlas-entity-tree--icon 'file rel) "") rel))
             (add-text-properties
              lstart (line-end-position 0)
              (list 'atlas-root (file-name-as-directory (expand-file-name root))
                    'atlas-rel rel
                    'mouse-face 'highlight)))
           ;; Symbols defined in this file
           (let* ((idxs (plist-get state :indexes))
                  (by-id (and idxs (plist-get idxs atlas-model--k-sym-by-id)))
                  (syms '()))
             (when (hash-table-p by-id)
               (maphash
                (lambda (_id s)
                  (when (equal (plist-get s :file) rel)
                    (push s syms)))
                by-id))
             ;; Stable sort: by :beg then by :name
             (setq syms
                   (seq-sort
                    (lambda (a b)
                      (let* ((ba (or (plist-get a :beg) 0))
                             (bb (or (plist-get b :beg) 0))
                             (na (or (plist-get a :name) ""))
                             (nb (or (plist-get b :name) "")))
                        (or (< ba bb)
                            (and (= ba bb) (string-lessp na nb)))))
                    syms))
             (let* ((total (length syms))
                    (limit 20)
                    (shown (if (> total limit) (seq-take syms limit) syms)))
               (insert (format "      Symbols (%d):\n" total))
               (dolist (s shown)
                 (let* ((name (or (plist-get s :name) "symbol"))
                        (kind (or (plist-get s :kind) 'symbol))
                        (sig  (or (plist-get s :sig) ""))
                        (doc1 (or (plist-get s :doc1) ""))
                        (beg  (or (plist-get s :beg) 1))
                        (kstr (capitalize (symbol-name kind)))
                        (lstart2 (point)))
                   (insert (format "        %s %s [%s]"
                                   (or (atlas-entity-tree--icon 'kind kind) "")
                                   name kstr))
                   (when (and (stringp sig) (> (length sig) 0))
                     (insert (format "  %s" sig)))
                   (when (and (stringp doc1) (> (length doc1) 0))
                     (let ((first (car (split-string doc1 "\n" t))))
                       (when (and first (> (length first) 0))
                         (insert (format " — %s" first)))))
                   (insert "\n")
                   ;; Make symbol line clickable (opens file at :beg)
                   (add-text-properties
                    lstart2 (line-end-position 0)
                    (list 'atlas-root (file-name-as-directory (expand-file-name root))
                          'atlas-rel rel
                          'atlas-beg beg
                          'mouse-face 'highlight))))
               (when (> total (length shown))
                 (insert (format "        … and %d more\n" (- total (length shown)))))))))
      (insert (format "Features (%d):\n" (length features)))
      (dolist (feat features)
        (let* ((provided (atlas-entity-tree--files-for-feature state feat 'provide))
               (required (atlas-entity-tree--files-for-feature state feat 'require)))
          ;; Feature header
          (insert (format "- %s %s\n" (or (atlas-entity-tree--icon 'feature feat) "") feat))
          ;; Providers
          (insert (format "  Provided by (%d):\n" (length provided)))
          (dolist (rel provided)
            (atlas-entity-tree--print-file-and-symbols rel))
          ;; Requirees
          (insert (format "  Required by (%d):\n" (length required)))
          (dolist (rel required)
            (atlas-entity-tree--print-file-and-symbols rel)))))))

(defun atlas-entity-tree--all-symbols (state)
  "Return list of symbol plists from STATE via inverted index."
  (let* ((inv (atlas-model-ensure-inv-index state))
         (seen (make-hash-table :test #'equal))
         (ids '())
         (syms '()))
    (when inv
      (maphash
       (lambda (_tkn vec)
         (dotimes (i (length vec))
           (let ((id (aref vec i)))
             (unless (gethash id seen)
               (puthash id t seen)
               (push id ids)))))
       inv))
    (dolist (id ids)
      (let ((s (atlas-model-get-symbol state id)))
        (when (and s (plist-get s :name))
          (push s syms))))
    (nreverse syms)))

(defun atlas-entity-tree--insert-by-kind (root)
  "Insert textual tree for 'by-kind view for ROOT."
  (let* ((state (atlas-entity-tree--state root))
         (syms (atlas-entity-tree--all-symbols state))
         (groups (let ((ht (make-hash-table :test #'eq)))
                   (dolist (k '(function macro var custom const symbol))
                     (puthash k '() ht))
                   ht)))
    (insert "Symbols by kind:\n")
    ;; Distribute symbols into groups
    (dolist (s syms)
      (let* ((k (or (plist-get s :kind) 'symbol))
             (lst (gethash k groups)))
        (puthash k (cons s lst) groups)))
    ;; Stable ordering: function, macro, var, custom, const, symbol
    (dolist (k '(function macro var custom const symbol))
      (let* ((lst (nreverse (gethash k groups)))
             (lst (seq-sort
                   (lambda (a b)
                     (let* ((na (or (plist-get a :name) ""))
                            (nb (or (plist-get b :name) ""))
                            (fa (or (plist-get a :file) ""))
                            (fb (or (plist-get b :file) "")))
                       (or (string-lessp na nb)
                           (and (string= na nb)
                                (string-lessp fa fb)))))
                   lst)))
        (when lst
          (insert (format "- %s (%d):\n"
                          (capitalize (symbol-name k)) (length lst)))
          (dolist (s lst)
            (let* ((name (or (plist-get s :name) "symbol"))
                   (rel (or (plist-get s :file) ""))
                   (beg (or (plist-get s :beg) 1))
                   (icon (or (atlas-entity-tree--icon 'kind (plist-get s :kind)) ""))
                   (lstart (point)))
              (insert (format "    %s %s  file=%s\n" icon name rel))
              (add-text-properties
               lstart (line-end-position 0)
               (list 'atlas-root (file-name-as-directory (expand-file-name root))
                     'atlas-rel rel
                     'atlas-beg beg
                     'mouse-face 'highlight)))))))))

(defvar-local atlas-entity-tree--search-query nil
  "Buffer-local search query for 'search view.")

(defcustom atlas-entity-tree-search-k 30
  "Max number of search results to show in entity tree search view."
  :type 'integer :group 'atlas-entity-tree)

(defvar-local atlas-entity-tree--plan-query nil
  "Buffer-local plan query for 'plan view.")

(defcustom atlas-entity-tree-plan-k 12
  "Default top K for plan view."
  :type 'integer :group 'atlas-entity-tree)

(defcustom atlas-entity-tree-plan-budget atlas-plan-default-budget
  "Default token budget for plan view."
  :type 'integer :group 'atlas-entity-tree)

(defun atlas-entity-tree--insert-search (root)
  "Insert 'search view for ROOT using atlas-entity-tree--search-query'."
  (let* ((query (or atlas-entity-tree--search-query ""))
         (k (or atlas-entity-tree-search-k 30))
         (state (atlas-entity-tree--state root))
         (results (ignore-errors (atlas-query root query :k k))))
    (insert (format "Search: %s  (k=%d results=%d)\n" query k (length (or results '()))))
    (insert "\n")
    (dolist (r (or results '()))
      (let* ((name (or (alist-get :name r) (alist-get :id r) "symbol"))
             (score (or (alist-get :score r) 0))
             (rel (or (alist-get :file r) ""))
             (range (alist-get :range r))
             (beg (or (car-safe range) 1)))
        (let* ((sym (and (alist-get :id r)
                         (atlas-model-get-symbol state (alist-get :id r))))
               (icon (or (atlas-entity-tree--icon 'kind (and sym (plist-get sym :kind))) "")))
          (insert (format "- %s %s  score=%s\n" icon name score)))
        (let ((lstart (point)))
          (insert (format "  %s %s\n" (or (atlas-entity-tree--icon 'file rel) "") rel))
          (add-text-properties
           lstart (line-end-position 0)
           (list 'atlas-root (file-name-as-directory (expand-file-name root))
                 'atlas-rel rel
                 'atlas-beg beg
                 'mouse-face 'highlight)))
        ;; Out edges (from file)
        (let* ((eout (and (stringp rel) (atlas-model-edges-out state rel)))
               (eout (or eout '())))
          (insert (format "  Out edges (%d):\n" (length eout)))
          (dolist (e eout)
            (let* ((type (format "%s" (plist-get e :type)))
                   (to   (format "%s" (plist-get e :to))))
              (insert (format "    %s -> " type))
              (let ((p1 (point)))
                (insert to)
                (add-text-properties
                 p1 (point)
                 (list 'atlas-edge-target to
                       'atlas-root (file-name-as-directory (expand-file-name root))
                       'mouse-face 'highlight
                       'help-echo (format "Jump to %s" to))))
              (insert "\n"))))
        ;; In edges (to file)
        (let* ((ein (and (stringp rel) (atlas-model-edges-in state rel)))
               (ein (or ein '())))
          (insert (format "  In edges (%d):\n" (length ein)))
          (dolist (e ein)
            (let* ((type (format "%s" (plist-get e :type)))
                   (from (format "%s" (plist-get e :from))))
              (insert (format "    %s <- " type))
              (let ((p1 (point)))
                (insert from)
                (add-text-properties
                 p1 (point)
                 (list 'atlas-edge-target from
                       'atlas-root (file-name-as-directory (expand-file-name root))
                       'mouse-face 'highlight
                       'help-echo (format "Jump to %s" from))))
              (insert "\n"))))
        (insert "\n")))))

(defun atlas-entity-tree--insert-imports (root)
  "Insert 'imports' view content for ROOT."
  (insert "Imports/Provides:\n")
  (atlas-entity-tree--insert-by-feature root))

(defun atlas-entity-tree--insert-edges (root)
  "Insert 'edges' view for ROOT centered around selector."
  (let* ((sel atlas-entity-tree--edges-selector)
         (depth (or atlas-entity-tree-graph-depth 1))
         (state (atlas-entity-tree--state root)))
    (insert (format "Edges around: %s  depth=%d\n" (or sel "<nil>") depth))
    (insert "\n")
    (if (not (and (stringp sel) (> (length sel) 0)))
        (insert "  No selector set. Use M-x atlas-entity-tree-edges to choose.\n")
      (let* ((out (or (atlas-model-edges-out state sel) '()))
             (in  (or (atlas-model-edges-in state sel) '())))
        (insert (format "  Out edges (%d):\n" (length out)))
        (dolist (e out)
          (let* ((type (format "%s" (plist-get e :type)))
                 (to   (format "%s" (plist-get e :to))))
            (insert (format "    %s -> " type))
            (let ((p1 (point)))
              (insert to)
              (add-text-properties
               p1 (point)
               (list 'atlas-edge-target to
                     'atlas-root (file-name-as-directory (expand-file-name root))
                     'mouse-face 'highlight
                     'help-echo (format "Jump to %s" to))))
            (insert "\n")))
        (insert (format "  In edges (%d):\n" (length in)))
        (dolist (e in)
          (let* ((type (format "%s" (plist-get e :type)))
                 (from (format "%s" (plist-get e :from))))
            (insert (format "    %s <- " type))
            (let ((p1 (point)))
              (insert from)
              (add-text-properties
               p1 (point)
               (list 'atlas-edge-target from
                     'atlas-root (file-name-as-directory (expand-file-name root))
                     'mouse-face 'highlight
                     'help-echo (format "Jump to %s" from))))
            (insert "\n")))))))

(defun atlas-entity-tree--insert-plan (root)
  "Insert 'plan view for ROOT using atlas-plan-context."
  (let* ((query (or atlas-entity-tree--plan-query ""))
         (k (or atlas-entity-tree-plan-k 12))
         (budget (or atlas-entity-tree-plan-budget atlas-plan-default-budget))
         (plan (ignore-errors (atlas-plan-context root query :k k :budget budget :model 'brief)))
         (files (and plan (alist-get :files plan)))
         (spans (and plan (alist-get :spans plan)))
         (est (or (and plan (alist-get :est-tokens plan)) 0)))
    ;; Fallback when plan returns no files: build from query + 1-hop edges (require -> providers)
    (when (or (null files) (= (length files) 0))
      (let* ((state (atlas-entity-tree--state root))
             (res (ignore-errors (atlas-query root query :k k)))
             (fht (make-hash-table :test #'equal))
             (acc-spans '()))
        (dolist (r (or res '()))
          (let* ((rel (alist-get :file r))
                 (range (alist-get :range r))
                 (beg (or (car-safe range) 0))
                 (end (or (cdr-safe range) 0))
                 (abs (expand-file-name rel (file-name-as-directory root)))
                 (se (atlas-plan--file-span-around abs beg end 3)))
            (when (stringp rel)
              (puthash rel t fht))
            (push (list :file rel :beg (plist-get se :beg) :end (plist-get se :end)) acc-spans)
            ;; 1-hop expansion to providers of required features
            (dolist (e (or (atlas-model-edges-out state rel) '()))
              (when (and (eq (plist-get e :type) 'require)
                         (stringp (plist-get e :to)))
                (dolist (pe (or (atlas-model-edges-in state (plist-get e :to)) '()))
                  (when (eq (plist-get pe :type) 'provide)
                    (let ((frel (plist-get pe :from)))
                      (when (stringp frel)
                        (puthash frel t fht)))))))))
        (let (lst)
          (maphash (lambda (k _v) (push k lst)) fht)
          (setq files (seq-sort #'string< (nreverse lst))))
        (setq spans (nreverse acc-spans))
        (setq est 0))) ;; keep est as 0 for fallback
    (insert (format "Plan: %s, k=%d, budget=%d  est=%d\n\n" query k budget est))
    (insert (format "Files (%d):\n" (length (or files '()))))
    (dolist (rel (or files '()))
      (let ((lstart (point)))
        (insert (format "- %s %s\n" (or (atlas-entity-tree--icon 'file rel) "") rel))
        (add-text-properties
         lstart (line-end-position 0)
         (list 'atlas-root (file-name-as-directory (expand-file-name root))
               'atlas-rel rel
               'mouse-face 'highlight)))
      (let ((fspans (seq-filter (lambda (sp) (equal (plist-get sp :file) rel))
                                (or spans '()))))
        (dolist (sp fspans)
          (let* ((beg (or (plist-get sp :beg) 1))
                 (end (or (plist-get sp :end) beg))
                 (lstart (point)))
            (insert (format "  span %d..%d\n" beg end))
            (add-text-properties
             lstart (line-end-position 0)
             (list 'atlas-root (file-name-as-directory (expand-file-name root))
                   'atlas-rel rel
                   'atlas-beg beg
                   'mouse-face 'highlight))))))
    (insert "\n")))

(defun atlas-entity-tree--insert-view (root)
  "Insert current view content for ROOT."
  (let ((view (or atlas-entity-tree--view atlas-entity-tree-default-view)))
    (pcase view
      ('by-feature
       (atlas-entity-tree--insert-by-feature root))
      ('by-kind
       (atlas-entity-tree--insert-by-kind root))
      ('search
       (atlas-entity-tree--insert-search root))
      ('imports
       (atlas-entity-tree--insert-imports root))
      ('edges
       (atlas-entity-tree--insert-edges root))
      ('plan
       (atlas-entity-tree--insert-plan root))
      (_
       ;; Placeholder for not-yet-implemented views
       (insert (format "View: %s (not implemented yet)\n" (symbol-name view)))
       (insert "- Press g to refresh\n")))))

(defun atlas-entity-tree--register-buffer (buf)
  "Register BUF into global set of entity tree buffers."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook #'atlas-entity-tree--on-kill nil t))
    (cl-pushnew buf atlas-entity-tree--buffers)))

(defun atlas-entity-tree--on-kill ()
  "Remove current buffer from registry on kill."
  (setq atlas-entity-tree--buffers (delq (current-buffer) atlas-entity-tree--buffers))
  t)

(defun atlas-entity-tree--ensure-subscribed ()
  "Ensure global event handlers are subscribed once."
  (when (null atlas-entity-tree--unsubs)
    (let ((u1 (atlas-events-subscribe :atlas-index-start #'atlas-entity-tree--on-index-start))
          (u2 (atlas-events-subscribe :atlas-index-progress #'atlas-entity-tree--on-index-progress))
          (u3 (atlas-events-subscribe :atlas-index-done #'atlas-entity-tree--on-index-done))
          (u4 (atlas-events-subscribe :atlas-index-error #'atlas-entity-tree--on-index-done)))
      (setq atlas-entity-tree--unsubs (list u1 u2 u3 u4))
      (atlas-log :debug "entity-tree: subscribed to atlas events"))))

(defun atlas-entity-tree--status-line (root)
  "Build status line for ROOT and buffer-local status."
  (let* ((st (or atlas-entity-tree--status ""))
         (r (or root "")))
    (string-trim (format "Atlas Entities — root: %s%s"
                         (if (stringp r) r "<nil>")
                         (if (and (stringp st) (> (length st) 0)) (format "   [%s]" st) "")))))

(defun atlas-entity-tree--render-text (root)
  "Render tree for ROOT using simple text fallback."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (atlas-entity-tree--status-line root) "\n\n")
    (atlas-entity-tree--insert-view root)
    (goto-char (point-min))))

(defun atlas-entity-tree--render (root)
  "Render tree using selected backend for ROOT."
  (pcase atlas-entity-tree-section-style
    ('magit-section
     (if (featurep 'magit-section)
         (progn
           (require 'magit-section)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (magit-insert-section (atlas-entity-tree-root)
               (magit-insert-heading (atlas-entity-tree--status-line root))
               (insert "\n")
               (magit-insert-section (atlas-entity-tree-view)
                 (magit-insert-heading
                   (format "View: %s"
                           (or (and atlas-entity-tree--view (symbol-name atlas-entity-tree--view))
                               (symbol-name atlas-entity-tree-default-view))))
                 (insert "\n")
                 (atlas-entity-tree--insert-view root))))
           (goto-char (point-min)))
       ;; Fallback to text if magit-section not truly available
       (atlas-entity-tree--render-text root)))
    (_ (atlas-entity-tree--render-text root))))

;;;###autoload
(defun atlas-entity-tree-refresh (&optional hard)
  "Refresh entity tree buffer. If HARD is non-nil, rebuild state (placeholder)."
  (interactive "P")
  (unless (derived-mode-p 'atlas-entity-tree-mode)
    (user-error "Not in atlas-entity-tree buffer"))
  (when hard
    ;; Future: clear caches/views here
    (atlas-log :debug "entity-tree: hard refresh requested"))
  (atlas-entity-tree--render atlas-entity-tree--root)
  (message "Atlas entity tree refreshed"))

(defun atlas-entity-tree--refresh-all ()
  "Refresh all registered buffers."
  (dolist (buf atlas-entity-tree--buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'atlas-entity-tree-mode)
          (atlas-entity-tree--render atlas-entity-tree--root))))))

(defun atlas-entity-tree--set-status (status)
  "Set STATUS for current buffer and re-render header."
  (setq atlas-entity-tree--status (or status ""))
  (atlas-entity-tree--render atlas-entity-tree--root))

(defun atlas-entity-tree--set-status-all (status)
  "Update STATUS for all registered buffers."
  (dolist (buf atlas-entity-tree--buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'atlas-entity-tree-mode)
          (setq atlas-entity-tree--status (or status ""))
          (atlas-entity-tree--render atlas-entity-tree--root))))))

(defun atlas-entity-tree--on-index-start (&rest _args)
  "Handle :atlas-index-start — set indexing status."
  (atlas-log :info "entity-tree: index start")
  (atlas-entity-tree--set-status-all "indexing…"))

(defun atlas-entity-tree--on-index-progress (&rest args)
  "Handle :atlas-index-progress — update counts in status."
  (let* ((f (plist-get args :files))
         (s (plist-get args :symbols))
         (e (plist-get args :edges))
         (txt (format "f=%s s=%s e=%s" (or f 0) (or s 0) (or e 0))))
    (atlas-entity-tree--set-status-all txt)))

(defun atlas-entity-tree--on-index-done (&rest _args)
  "Handle :atlas-index-done/error — clear status and maybe refresh."
  (atlas-log :info "entity-tree: index done")
  (atlas-entity-tree--set-status-all "")
  (when atlas-entity-tree-auto-refresh
    (atlas-entity-tree--refresh-all)))

(defun atlas-entity-tree--open-file (root rel beg)
  "Open REL under ROOT and go to BEG. If file does not exist, visit a new buffer without creating it on disk."
  (let* ((abs (expand-file-name rel (file-name-as-directory root))))
    (find-file abs)
    (goto-char (or beg 1))
    (recenter)))

;;;###autoload
(defun atlas-entity-tree-open-at-point ()
  "Open entity or edge target at point using text properties.
If on an edge target, jump accordingly; otherwise open file at position."
  (interactive)
  (let* ((pos (point))
         (edge (or (get-text-property pos 'atlas-edge-target)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-edge-target))))
         (root (or (get-text-property pos 'atlas-root)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-root))
                   atlas-entity-tree--root))
         (rel  (or (get-text-property pos 'atlas-rel)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-rel))))
         (beg  (or (get-text-property pos 'atlas-beg)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-beg)))))
    (cond
     (edge
      (cond
       ((and (stringp edge) (string-prefix-p "feature:" edge))
        (setq-local atlas-entity-tree--view 'edges)
        (setq-local atlas-entity-tree--edges-selector edge)
        (atlas-entity-tree-refresh))
       ((and (stringp edge) (string-match-p "\\.el\\'" edge))
        (atlas-entity-tree--open-file root edge 1))
       ((stringp edge)
        (let* ((state (atlas-entity-tree--state (or root atlas-entity-tree--root)))
               (sym (and state (atlas-model-get-symbol state edge)))
               (srel (and sym (plist-get sym :file)))
               (sbeg (and sym (plist-get sym :beg))))
          (if (and srel)
              (atlas-entity-tree--open-file (or root atlas-entity-tree--root) srel sbeg)
            (message "Unknown edge target: %s" edge))))
       (t (message "No openable edge at point"))))
     ((and root rel)
      (atlas-entity-tree--open-file root rel beg))
     (t
      (message "No openable item at point")))))

;;;###autoload
(defun atlas-entity-tree-peek-at-point ()
  "Peek content of file at point in a side window. Smoke implementation."
  (interactive)
  (let* ((pos (point))
         (rel  (or (get-text-property pos 'atlas-rel)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-rel))))
         (root (or (get-text-property pos 'atlas-root)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-root)))))
    (if (and root rel)
        (let* ((abs (expand-file-name rel (file-name-as-directory root)))
               (buf (find-file-noselect abs)))
          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (let ((win (display-buffer-in-side-window
                          (current-buffer)
                          '((side . bottom) (slot . -1)))))
                (when (window-live-p win)
                  (set-window-text-height win atlas-entity-tree-peek-window-height)))))
          t)
      (progn (message "No previewable item at point") nil))))

;;;###autoload
(defun atlas-entity-tree-copy-at-point ()
  "Copy item path at point (atlas-rel) to kill-ring. Return copied string or nil."
  (interactive)
  (let* ((pos (point))
         (rel (or (get-text-property pos 'atlas-rel)
                  (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-rel)))))
    (if (and (stringp rel) (> (length rel) 0))
        (progn
          (kill-new rel)
          (message "Copied: %s" rel)
          rel)
      (message "No copyable item at point")
      nil)))

(defcustom atlas-entity-tree-use-transient t
  "If non-nil and transient is available, use a transient menu for actions."
  :type 'boolean :group 'atlas-entity-tree)

;; Optional transient-based menu
(when (require 'transient nil 'noerror)
  (transient-define-suffix atlas-entity-tree--t-open ()
    "Open at point."
    :description "Open"
    (interactive) (atlas-entity-tree-open-at-point))

  (transient-define-suffix atlas-entity-tree--t-peek ()
    "Peek at point."
    :description "Peek"
    (interactive) (atlas-entity-tree-peek-at-point))

  (transient-define-suffix atlas-entity-tree--t-copy ()
    "Copy path at point."
    :description "Copy path"
    (interactive) (atlas-entity-tree-copy-at-point))

  (transient-define-suffix atlas-entity-tree--t-set-view (view)
    "Set current view to VIEW."
    :description (lambda () (format "Set view: %s" (or (and (boundp 'view) view) "<ask>")))
    (interactive (list (intern (completing-read "View: " '("by-feature" "by-kind" "search" "imports" "edges" "plan") nil t))))
    (atlas-entity-tree-set-view view))

  (transient-define-suffix atlas-entity-tree--t-search ()
    "Search view (prompt)."
    :description "Search…"
    (interactive)
    (let* ((root (or atlas-entity-tree--root default-directory))
           (q (read-string "Search query: " (or atlas-entity-tree--search-query ""))))
      (atlas-entity-tree-search root q)))

  (transient-define-suffix atlas-entity-tree--t-edges ()
    "Edges view around selector (prompt)."
    :description "Edges around…"
    (interactive)
    (let* ((root (or atlas-entity-tree--root default-directory))
           (sel (read-string "Selector (file REL | feature:NAME | symbol id): " (or atlas-entity-tree--edges-selector "")))
           (depth (read-number "Depth: " (or atlas-entity-tree-graph-depth 1))))
      (atlas-entity-tree-edges root sel depth)))

  (transient-define-suffix atlas-entity-tree--t-plan ()
    "Plan view (prompt)."
    :description "Plan…"
    (interactive)
    (let* ((root (or atlas-entity-tree--root default-directory))
           (q (read-string "Plan query: " (or atlas-entity-tree--plan-query "")))
           (k (read-number "Top K: " (or atlas-entity-tree-plan-k 12)))
           (budget (read-number "Budget: " (or atlas-entity-tree-plan-budget atlas-plan-default-budget))))
      (atlas-entity-tree-plan root q k budget)))

  (transient-define-suffix atlas-entity-tree--t-follow-toggle ()
    "Toggle follow-mode."
    :description "Toggle follow-mode"
    (interactive) (atlas-entity-tree-toggle-follow))

  (transient-define-suffix atlas-entity-tree--t-refresh ()
    "Refresh view."
    :description "Refresh"
    (interactive) (atlas-entity-tree-refresh))

  (transient-define-prefix atlas-entity-tree-transient ()
    "Atlas Entities — actions"
    [["Actions"
      ("o" atlas-entity-tree--t-open)
      ("p" atlas-entity-tree--t-peek)
      ("y" atlas-entity-tree--t-copy)]
     ["Views"
      ("v f" "by-feature" (lambda () (interactive) (atlas-entity-tree-set-view 'by-feature)))
      ("v k" "by-kind"    (lambda () (interactive) (atlas-entity-tree-set-view 'by-kind)))
      ("v s" "search…"    atlas-entity-tree--t-search)
      ("v i" "imports"    (lambda () (interactive) (atlas-entity-tree-set-view 'imports)))
      ("v e" "edges…"     atlas-entity-tree--t-edges)
      ("v p" "plan…"      atlas-entity-tree--t-plan)]
     ["Misc"
      ("g" atlas-entity-tree--t-refresh)
      ("i" atlas-entity-tree--t-follow-toggle)]]))

;;;###autoload
(defun atlas-entity-tree-search-command ()
  "Prompt and open 'search view for the current entity tree buffer."
  (interactive)
  (unless (derived-mode-p 'atlas-entity-tree-mode)
    (user-error "Not in atlas-entity-tree buffer"))
  (let* ((root (or atlas-entity-tree--root default-directory))
         (q (read-string "Search query: " (or atlas-entity-tree--search-query ""))))
    (atlas-entity-tree-search root q)))

;;;###autoload
(defun atlas-entity-tree-edges-command ()
  "Prompt and open 'edges view for the current entity tree buffer."
  (interactive)
  (unless (derived-mode-p 'atlas-entity-tree-mode)
    (user-error "Not in atlas-entity-tree buffer"))
  (let* ((root (or atlas-entity-tree--root default-directory))
         (sel (read-string "Selector (file REL | feature:NAME | symbol id): " (or atlas-entity-tree--edges-selector "")))
         (depth (read-number "Depth: " (or atlas-entity-tree-graph-depth 1))))
    (atlas-entity-tree-edges root sel depth)))

;;;###autoload
(defun atlas-entity-tree-plan-command ()
  "Prompt and open 'plan view for the current entity tree buffer."
  (interactive)
  (unless (derived-mode-p 'atlas-entity-tree-mode)
    (user-error "Not in atlas-entity-tree buffer"))
  (let* ((root (or atlas-entity-tree--root default-directory))
         (q (read-string "Plan query: " (or atlas-entity-tree--plan-query "")))
         (k (read-number "Top K: " (or atlas-entity-tree-plan-k 12)))
         (budget (read-number "Budget: " (or atlas-entity-tree-plan-budget atlas-plan-default-budget))))
    (atlas-entity-tree-plan root q k budget)))

;;;###autoload
(defun atlas-entity-tree-actions ()
  "Show actions menu for item at point.
If =atlas-entity-tree-use-transient' and transient is available, show transient menu.
Otherwise, fallback to a simple completing-read menu."
  (interactive)
  (if (and atlas-entity-tree-use-transient (featurep 'transient) (fboundp 'atlas-entity-tree-transient))
      (atlas-entity-tree-transient)
    (let* ((choices '("Open" "Peek" "Copy path" "Search…" "Edges…" "Plan…" "Set view…"))
           (act (completing-read "Action: " choices nil t)))
      (pcase act
        ("Open" (call-interactively #'atlas-entity-tree-open-at-point))
        ("Peek" (call-interactively #'atlas-entity-tree-peek-at-point))
        ("Copy path" (call-interactively #'atlas-entity-tree-copy-at-point))
        ("Search…" (call-interactively #'atlas-entity-tree-search-command))
        ("Edges…" (call-interactively #'atlas-entity-tree-edges-command))
        ("Plan…" (call-interactively #'atlas-entity-tree-plan-command))
        ("Set view…"
         (call-interactively #'atlas-entity-tree-set-view))))))

(defun atlas-entity-tree--follow-post-command ()
  "Internal: in follow-mode, auto-peek item under point. Returns t."
  (when (and (derived-mode-p 'atlas-entity-tree-mode)
             atlas-entity-tree-follow-mode)
    (ignore-errors (atlas-entity-tree-peek-at-point)))
  t)

;;;###autoload
(define-minor-mode atlas-entity-tree-follow-mode
  "Toggle follow-mode in Atlas entity tree: auto-peek as point moves."
  :init-value nil
  :lighter " Follow"
  :group 'atlas-entity-tree
  (if atlas-entity-tree-follow-mode
      (add-hook 'post-command-hook #'atlas-entity-tree--follow-post-command nil t)
    (remove-hook 'post-command-hook #'atlas-entity-tree--follow-post-command t)))

;;;###autoload
(defun atlas-entity-tree-toggle-follow ()
  "Toggle follow-mode in current entity tree buffer."
  (interactive)
  (if (not (derived-mode-p 'atlas-entity-tree-mode))
      (user-error "Not in atlas-entity-tree buffer")
    (if atlas-entity-tree-follow-mode
        (atlas-entity-tree-follow-mode -1)
      (atlas-entity-tree-follow-mode +1))
    (message "atlas-entity-tree follow-mode: %s"
             (if atlas-entity-tree-follow-mode "on" "off"))))

(defvar atlas-entity-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g")   #'atlas-entity-tree-refresh)
    (define-key map (kbd "RET") #'atlas-entity-tree-open-at-point)
    (define-key map (kbd "o")   #'atlas-entity-tree-open-at-point)
    (define-key map (kbd "p")   #'atlas-entity-tree-peek-at-point)
    (define-key map (kbd "d")   #'atlas-entity-tree-toggle-docstring-at-point)
    (define-key map (kbd "i")   #'atlas-entity-tree-toggle-follow)
    (define-key map (kbd "a")   #'atlas-entity-tree-actions)
    ;; quick commands for views
    (define-key map (kbd "s")   #'atlas-entity-tree-search-command)
    (define-key map (kbd "E")   #'atlas-entity-tree-edges-command)
    (define-key map (kbd "P")   #'atlas-entity-tree-plan-command)
    map)
  "Keymap for =atlas-entity-tree-mode'.")

;;;###autoload
(define-derived-mode atlas-entity-tree-mode special-mode "Atlas-Entities"
  "Major mode for Atlas entity tree visualization.
Keys:
  g       refresh
  RET/o   open item
  p       peek item
  s       search view (prompt)
  E       edges view (prompt)
  P       plan view (prompt)
  i       toggle follow-mode (auto-peek on cursor move)
  a       actions menu (transient if available; fallback to simple menu)"
  :group 'atlas-entity-tree
  (setq buffer-read-only t))

;;;###autoload
(cl-defun atlas-entity-tree (root)
  "Open or refresh the Atlas entity tree for ROOT directory.
Returns the buffer."
  (interactive (list (read-directory-name "Atlas root: " nil nil t)))
  (let* ((state (or (atlas-state root) (atlas-open root)))
         (buf (get-buffer-create atlas-entity-tree--buffer-name)))
    (unless state
      (user-error "Atlas is not available for root: %s" root))
    (with-current-buffer buf
      (atlas-entity-tree-mode)
      (setq-local atlas-entity-tree--root (file-name-as-directory (expand-file-name root)))
      (unless atlas-entity-tree--view
        (setq-local atlas-entity-tree--view atlas-entity-tree-default-view))
      (atlas-entity-tree--ensure-subscribed)
      (atlas-entity-tree--register-buffer (current-buffer))
      (atlas-entity-tree--render atlas-entity-tree--root))
    (pop-to-buffer buf)
    buf))

;;;###autoload
(defun atlas-entity-tree-search (root query &optional k)
  "Open Atlas entity tree for ROOT in 'search view with QUERY and optional K limit.
Returns the tree buffer."
  (interactive
   (list (read-directory-name "Atlas root: " nil nil t)
         (read-string "Query: ")
         (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (let ((buf (atlas-entity-tree root)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local atlas-entity-tree--view 'search)
        (setq-local atlas-entity-tree--search-query (or query ""))
        (when (and k (integerp k) (> k 0))
          (setq-local atlas-entity-tree-search-k k))
        (atlas-entity-tree-refresh)))
    buf))

;;;###autoload
(defun atlas-entity-tree-edges (root selector &optional depth)
  "Open Atlas entity tree for ROOT in 'edges view around SELECTOR.
SELECTOR may be file REL, feature:NAME, or symbol id. With prefix arg, set DEPTH."
  (interactive
   (list (read-directory-name "Atlas root: " nil nil t)
         (read-string "Selector (file REL, feature:NAME, or symbol id): ")
         (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (let ((buf (atlas-entity-tree root)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local atlas-entity-tree--view 'edges)
        (setq-local atlas-entity-tree--edges-selector (or selector ""))
        (when (and depth (integerp depth) (> depth 0))
          (setq-local atlas-entity-tree-graph-depth depth))
        (atlas-entity-tree-refresh)))
    buf))

;;;###autoload
(defun atlas-entity-tree-plan (root query &optional k budget)
  "Open Atlas entity tree for ROOT in 'plan view for QUERY.
Optional K and BUDGET override defaults for the view."
  (interactive
   (list (read-directory-name "Atlas root: " nil nil t)
         (read-string "Query: ")
         (when current-prefix-arg (read-number "Top K: " 12))
         (when current-prefix-arg (read-number "Budget (tokens): " atlas-plan-default-budget))))
  (let ((buf (atlas-entity-tree root)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local atlas-entity-tree--view 'plan)
        (setq-local atlas-entity-tree--plan-query (or query ""))
        (when (and k (integerp k) (> k 0))
          (setq-local atlas-entity-tree-plan-k k))
        (when (and budget (integerp budget) (> budget 0))
          (setq-local atlas-entity-tree-plan-budget budget))
        (atlas-entity-tree-refresh)))
    buf))

;;;###autoload
(defun atlas-entities (&optional root)
  "Compatibility alias: open Atlas Entities tree for ROOT (or current directory).
Replaces legacy =atlas-entities' UI with the new entity-centric tree."
  (interactive)
  (atlas-entity-tree (or root default-directory)))

;; Docstring helpers and toggle

(defun atlas-entity-tree--truncate-doc (s)
  "Truncate docstring S to =atlas-entity-tree-doc-max-lines' lines and add ellipsis."
  (let* ((max-lines (or (and (boundp 'atlas-entity-tree-doc-max-lines) atlas-entity-tree-doc-max-lines) 12))
         (ellipsis (or (and (boundp 'atlas-entity-tree-doc-ellipsis) atlas-entity-tree-doc-ellipsis) "…"))
         (lines (and (stringp s) (split-string (or s "") "\n"))))
    (if (and lines (> (length lines) max-lines))
        (string-join (append (cl-subseq lines 0 max-lines) (list ellipsis)) "\n")
      (or s ""))))

(defun atlas-entity-tree--symbol-docstring (root rel beg)
  "Extract full docstring for def/ at BEG in REL under ROOT, or nil."
  (condition-case _
      (let* ((abs (expand-file-name rel (file-name-as-directory root))))
        (when (file-exists-p abs)
          (with-temp-buffer
            (insert-file-contents abs)
            (goto-char (min (max 1 (or beg 1)) (point-max)))
            (let ((form (read (current-buffer))))
              (when (consp form)
                (cond
                 ((memq (car form) '(defun defmacro))
                  (let ((maybe (nth 3 form)))
                    (and (stringp maybe) maybe)))
                 ((memq (car form) '(defvar defcustom defconst))
                  (seq-find #'stringp (cdr form)))
                 (t nil)))))))
    (error nil)))

;;;###autoload
(defun atlas-entity-tree-toggle-docstring-at-point ()
  "Toggle inline docstring overlay for the symbol at point."
  (interactive)
  (let* ((pos (point))
         (root (or (get-text-property pos 'atlas-root)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-root))
                   atlas-entity-tree--root))
         (rel  (or (get-text-property pos 'atlas-rel)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-rel))))
         (beg  (or (get-text-property pos 'atlas-beg)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-beg))))
         (eol (line-end-position))
         (ov (seq-find (lambda (o) (overlay-get o 'atlas-doc))
                       (append (overlays-at eol)
                               (and (> eol (point-min)) (overlays-at (1- eol)))))))
    (cond
     (ov
      (delete-overlay ov)
      (message "Docstring hidden"))
     ((and root rel beg)
      (let* ((key (cons rel beg))
             (doc (or (and (hash-table-p atlas-entity-tree--doc-cache)
                           (gethash key atlas-entity-tree--doc-cache))
                      (let ((d (atlas-entity-tree--symbol-docstring root rel beg)))
                        (when (and (hash-table-p atlas-entity-tree--doc-cache) key)
                          (puthash key d atlas-entity-tree--doc-cache))
                        d)))
             (doc (atlas-entity-tree--truncate-doc (or doc "")))
             (indent (save-excursion
                       (back-to-indentation)
                       (make-string (+ (current-column) 2) ?\s)))
             (render
              (concat "\n"
                      (mapconcat (lambda (line)
                                   (concat indent line))
                                 (and (stringp doc) (split-string doc "\n" t))
                                 "\n"))))
        (let ((ov2 (make-overlay eol eol)))
          (overlay-put ov2 'after-string (propertize render 'face 'font-lock-doc-face))
          (overlay-put ov2 'atlas-doc t))
        (message "Docstring shown")))
     (t
      (message "No symbol with doc at point")))))

(provide 'atlas-entity-tree)

;;; atlas-entity-tree.el ends here
