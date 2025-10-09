;;; atlas.el --- Project map for Emacs: core and commands -*- lexical-binding: t; -*-

;; Author: Peter Kosov <11111000000@email.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/11111000000/atlas

;;; Commentary:
;; Atlas core: state, configuration, lifecycle, statistics, and public commands.
;; Clean core + thin ports. Data first, effectful edges isolated.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'subr-x)

;; Forward decls to avoid cycles
(declare-function atlas-store-load-meta "atlas-store" (root))
(declare-function atlas-store-save-meta "atlas-store" (root meta))
(declare-function atlas-run-sources "atlas-sources"
                  (root &key changed opts emit done kinds levels languages))
(declare-function atlas-query "atlas-query"
                  (root keywords &rest keys))
(declare-function atlas-events-publish "atlas-events" (topic &rest args))

(defgroup atlas nil
  "Universal project map for Emacs."
  :group 'tools
  :prefix "atlas-")

;; Storage/config
(defcustom atlas-index-ttl 600.0
  "Time-to-live in seconds before re-index is considered stale."
  :type 'number :group 'atlas)

(defcustom atlas-exclude-dirs '("\\.git\\'" "node_modules\\'" "build\\'" "dist\\'" "vendor\\'")
  "List of directory regexps to exclude during inventory."
  :type '(repeat regexp) :group 'atlas)

(defcustom atlas-max-file-size (* 1024 512)
  "Max file size in bytes to parse deeply; above this, degrade."
  :type 'integer :group 'atlas)

(defcustom atlas-hash-content nil
  "If non-nil, compute content hashes for files."
  :type 'boolean :group 'atlas)

(defcustom atlas-store-compressed nil
  "If non-nil, write compressed .sexp.gz store files."
  :type 'boolean :group 'atlas)

(defcustom atlas-segment-threshold 10000
  "Threshold at which store files split into segments."
  :type 'integer :group 'atlas)

;; Quality/balance
(defcustom atlas-elisp-use-elisp-refs nil
  "If non-nil, use elisp-refs for call/ref edges with limits."
  :type 'boolean :group 'atlas)

(defcustom atlas-elisp-refs-max-size (* 1024 256)
  "Max file size for elisp-refs processing."
  :type 'integer :group 'atlas)

(defcustom atlas-debounce-interval 0.2
  "Debounce interval for async tasks (seconds)."
  :type 'number :group 'atlas)

(defcustom atlas-parallel-limit 4
  "Max parallel tasks used by providers."
  :type 'integer :group 'atlas)

;; Planning
(defcustom atlas-plan-default-budget 1200
  "Default token budget for context planning."
  :type 'integer :group 'atlas)

(defcustom atlas-plan-model 'brief
  "Default plan model kind to generate."
  :type '(choice (const brief) (const rich) (symbol)) :group 'atlas)

;; Internal state
(defvar atlas--states (make-hash-table :test #'equal)
  "Root path → state alist with keys:
  :root :dir :meta :indexes :caches :last-index-at :inv-index-ready?.")

(defun atlas-root-dir (root)
  "Return Atlas storage dir for ROOT."
  (file-name-as-directory
   (expand-file-name ".context/atlas/v1/" (file-name-as-directory root))))

(defun atlas--now () (float-time (current-time)))

(cl-defun atlas--default-meta (root)
  "Build default meta alist for ROOT."
  (list :schema 1
        :project-root (file-name-as-directory (expand-file-name root))
        :generated-at (atlas--now)
        :counts (list :files 0 :symbols 0 :edges 0)
        :languages '(elisp)
        :opts (list :segment-threshold atlas-segment-threshold
                    :compressed? (and atlas-store-compressed t))))

(defun atlas--ensure-dir (dir)
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun atlas-state (root)
  "Return Atlas state for ROOT or nil."
  (gethash (file-name-as-directory (expand-file-name root)) atlas--states))

(defun atlas--set-state (root state)
  (puthash (file-name-as-directory (expand-file-name root)) state atlas--states))

(defun atlas-open (root)
  "Open Atlas for ROOT directory: ensure store dir, load meta, return state."
  (interactive (list (read-directory-name "Atlas root: " nil nil t)))
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (dir (atlas-root-dir root)))
    (atlas--ensure-dir dir)
    (let* ((meta (or (ignore-errors (atlas-store-load-meta root))
                     (atlas--default-meta root)))
           (state (or (atlas-state root)
                      (list :root root
                            :dir dir
                            :meta meta
                            :indexes (list) ; plists for in-memory indices
                            :caches (list)
                            :last-index-at 0.0
                            :inv-index-ready? nil))))
      (atlas--set-state root state)
      (when (called-interactively-p 'interactive)
        (message "Atlas opened at %s" dir))
      state)))

(defun atlas-close (root)
  "Close Atlas for ROOT: drop in-memory state."
  (interactive (list (read-directory-name "Atlas root: " nil nil t)))
  (let* ((root (file-name-as-directory (expand-file-name root))))
    (remhash root atlas--states)
    (when (called-interactively-p 'interactive)
      (message "Atlas closed for %s" root))
    t))

(defun atlas--update-meta-counts (state files symbols edges)
  (let* ((meta (plist-get state :meta))
         (counts (list :files (or files (plist-get (plist-get meta :counts) :files))
                       :symbols (or symbols (plist-get (plist-get meta :counts) :symbols))
                       :edges (or edges (plist-get (plist-get meta :counts) :edges)))))
    (plist-put meta :counts counts)
    (plist-put meta :generated-at (atlas--now))
    (plist-put state :meta meta)
    state))

(defun atlas-stats (root)
  "Return stats alist for ROOT with counts and schema."
  (interactive (list (read-directory-name "Atlas root: " nil nil t)))
  (let* ((state (or (atlas-state root) (atlas-open root)))
         (meta (plist-get state :meta))
         (counts (or (plist-get meta :counts) (list :files 0 :symbols 0 :edges 0)))
         (res (list :files (plist-get counts :files)
                    :symbols (plist-get counts :symbols)
                    :edges (plist-get counts :edges)
                    :t-indexed (- (atlas--now) (or (plist-get state :last-index-at) 0.0))
                    :schema (plist-get meta :schema))))
    (when (called-interactively-p 'interactive)
      (message "Atlas stats: files=%s symbols=%s edges=%s schema=%s"
               (plist-get res :files) (plist-get res :symbols)
               (plist-get res :edges) (plist-get res :schema)))
    res))

;; ID generation
(cl-defun atlas--symbol-id (&key lang rel name beg end kind)
  "Build stable symbol id string from parts."
  (let ((L (or lang "elisp"))
        (R (or rel ""))
        (N (or name ""))
        (B (number-to-string (or beg 0)))
        (E (number-to-string (or end 0)))
        (K (or kind "function")))
    (format "%s:%s#%s@%s-%s/%s" L R N B E K)))

;; Basic commands

;;;###autoload
(defun atlas-index (root &optional full)
  "Index ROOT. If FULL non-nil, force complete rebuild."
  (interactive (list (read-directory-name "Atlas root: " nil nil t)
                     (when current-prefix-arg t)))
  (let* ((state (atlas-open root))
         (counts (list :files 0 :symbols 0 :edges 0))
         (files-acc 0) (symbols-acc 0) (edges-acc 0)
         (emit (lambda (batch)
                 ;; batch: (:files LIST) (:symbols LIST) (:edges LIST) (:summaries LIST)
                 (let ((fs (alist-get :files batch))
                       (ss (alist-get :symbols batch))
                       (es (alist-get :edges batch)))
                   (cl-incf files-acc (length fs))
                   (cl-incf symbols-acc (length ss))
                   (cl-incf edges-acc (length es))
                   (atlas-events-publish :atlas-index-progress
                                         :files files-acc :symbols symbols-acc :edges edges-acc)
                   ;; In-memory merge hook would go here
                   )))
         (done (lambda ()
                 (setf counts (list :files files-acc :symbols symbols-acc :edges edges-acc)))))
    (atlas-events-publish :atlas-index-start :root root :full (and full t))
    (atlas-run-sources root :changed (unless full :auto) :opts nil :emit emit :done done
                       :kinds '(files symbols edges summaries) :levels '(L0 L1 L2 L3) :languages '(elisp))
    (funcall done)
    (setf state (atlas--update-meta-counts state files-acc symbols-acc edges-acc))
    (atlas-store-save-meta root (plist-get state :meta))
    (plist-put state :last-index-at (atlas--now))
    (atlas--set-state root state)
    (atlas-events-publish :atlas-index-done :root root :counts (plist-get state :meta))
    (when (called-interactively-p 'interactive)
      (message "Atlas indexed: files=%d symbols=%d edges=%d" files-acc symbols-acc edges-acc))
    (list :files files-acc :symbols symbols-acc :edges edges-acc :schema (plist-get (plist-get state :meta) :schema))))

;;;###autoload
(defun atlas-reindex-changed (root)
  "Reindex files that changed in ROOT since last run."
  (interactive (list (read-directory-name "Atlas root: " nil nil t)))
  (atlas-index root nil))

;;;###autoload
(defun atlas-query-command (root query &optional k)
  "Interactive query in ROOT for QUERY with top K results."
  (interactive (list (read-directory-name "Atlas root: " nil nil t)
                     (read-string "Query: ")
                     (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (let* ((k (or k 10))
         (res (atlas-query root query :k k)))
    (message "Top %d results returned" (length res))
    res))

(provide 'atlas)

;;; atlas.el ends here
