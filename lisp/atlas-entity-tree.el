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
  "If non-nil, show icons (future use)."
  :type 'boolean
  :group 'atlas-entity-tree)

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
    (insert (format "Features (%d):\n" (length features)))
    (dolist (feat features)
      (let* ((provided (atlas-entity-tree--files-for-feature state feat 'provide))
             (required (atlas-entity-tree--files-for-feature state feat 'require)))
        ;; Feature header
        (insert (format "- %s\n" feat))
        ;; Providers
        (insert (format "  Provided by (%d):\n" (length provided)))
        (dolist (rel provided)
          (let ((lstart (point)))
            (insert (format "    %s\n" rel))
            (add-text-properties
             lstart (line-end-position 0)
             (list 'atlas-root (file-name-as-directory (expand-file-name root))
                   'atlas-rel rel
                   'mouse-face 'highlight))))
        ;; Requirees
        (insert (format "  Required by (%d):\n" (length required)))
        (dolist (rel required)
          (let ((lstart (point)))
            (insert (format "    %s\n" rel))
            (add-text-properties
             lstart (line-end-position 0)
             (list 'atlas-root (file-name-as-directory (expand-file-name root))
                   'atlas-rel rel
                   'mouse-face 'highlight))))))))

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
                   (lstart (point)))
              (insert (format "    %s  file=%s\n" name rel))
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
        (insert (format "- %s  score=%s\n" name score))
        (let ((lstart (point)))
          (insert (format "  file=%s\n" rel))
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
            (insert (format "    %s -> %s\n"
                            (format "%s" (plist-get e :type))
                            (format "%s" (plist-get e :to))))))
        ;; In edges (to file)
        (let* ((ein (and (stringp rel) (atlas-model-edges-in state rel)))
               (ein (or ein '())))
          (insert (format "  In edges (%d):\n" (length ein)))
          (dolist (e ein)
            (insert (format "    %s <- %s\n"
                            (format "%s" (plist-get e :type))
                            (format "%s" (plist-get e :from))))))
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
          (insert (format "    %s -> %s\n"
                          (format "%s" (plist-get e :type))
                          (format "%s" (plist-get e :to)))))
        (insert (format "  In edges (%d):\n" (length in)))
        (dolist (e in)
          (insert (format "    %s <- %s\n"
                          (format "%s" (plist-get e :type))
                          (format "%s" (plist-get e :from)))))))))

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
        (insert (format "- %s\n" rel))
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

(defun atlas-entity-tree-open-at-point ()
  "Open entity at point (file for now) using text properties."
  (interactive)
  (let* ((pos (point))
         (rel  (or (get-text-property pos 'atlas-rel)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-rel))))
         (root (or (get-text-property pos 'atlas-root)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-root))))
         (beg  (or (get-text-property pos 'atlas-beg)
                   (and (> pos (point-min)) (get-text-property (1- pos) 'atlas-beg)))))
    (if (and root rel)
        (atlas-entity-tree--open-file root rel beg)
      (message "No openable item at point"))))

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

(defun atlas-entity-tree-actions ()
  "Show simple actions menu for item at point."
  (interactive)
  (let* ((choices '("Open" "Peek" "Copy path"))
         (act (completing-read "Action: " choices nil t)))
    (pcase act
      ("Open" (call-interactively #'atlas-entity-tree-open-at-point))
      ("Peek" (call-interactively #'atlas-entity-tree-peek-at-point))
      ("Copy path" (call-interactively #'atlas-entity-tree-copy-at-point)))))

(defun atlas-entity-tree--follow-post-command ()
  "Internal: in follow-mode, auto-peek item under point. Returns t."
  (when (and (derived-mode-p 'atlas-entity-tree-mode)
             atlas-entity-tree-follow-mode)
    (ignore-errors (atlas-entity-tree-peek-at-point)))
  t)

(define-minor-mode atlas-entity-tree-follow-mode
  "Toggle follow-mode in Atlas entity tree: auto-peek as point moves."
  :init-value nil
  :lighter " Follow"
  :group 'atlas-entity-tree
  (if atlas-entity-tree-follow-mode
      (add-hook 'post-command-hook #'atlas-entity-tree--follow-post-command nil t)
    (remove-hook 'post-command-hook #'atlas-entity-tree--follow-post-command t)))

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
    (define-key map (kbd "i")   #'atlas-entity-tree-toggle-follow)
    (define-key map (kbd "a")   #'atlas-entity-tree-actions)
    map)
  "Keymap for =atlas-entity-tree-mode'.")

(define-derived-mode atlas-entity-tree-mode special-mode "Atlas-Entities"
  "Major mode for Atlas entity tree visualization.
Keys:
  g       refresh
  RET/o   open item
  p       peek item
  i       toggle follow-mode (auto-peek on cursor move)
  a       actions menu"
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

(provide 'atlas-entity-tree)

;;; atlas-entity-tree.el ends here
