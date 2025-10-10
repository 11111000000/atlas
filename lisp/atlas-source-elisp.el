;;; atlas-source-elisp.el --- Elisp provider (L0-L2 skeleton) -*- lexical-binding: t; -*-

;;; Commentary:
;; L0: inventory of .el files
;; L1: basic symbols: defun/defmacro/defvar/defcustom/defconst (names only, positions 0..0 as stub)
;; L2: require/provide edges (naive scan)
;; Parsing is conservative and robust; large files degrade gracefully.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'atlas-log)

(declare-function atlas-root-dir "atlas" (root))
(declare-function atlas--symbol-id "atlas" (&key lang rel name beg end kind))

(defgroup atlas-elisp nil
  "Elisp source for Atlas."
  :group 'atlas
  :prefix "atlas-elisp-")

(defun atlas-elisp--excluded-dir-p (path)
  "Return non-nil if PATH is under an excluded directory according to atlas-exclude-dirs."
  (let* ((dir (file-name-directory path))
         (components (and dir (split-string (directory-file-name dir) "/" t))))
    (seq-some
     (lambda (re)
       (seq-some (lambda (d) (string-match-p re d)) components))
     (bound-and-true-p atlas-exclude-dirs))))

(defun atlas-elisp--list-files (root)
  "List .el files under ROOT respecting excludes."
  (let ((files nil))
    (dolist (path (directory-files-recursively root "\\.el\\'" nil))
      (unless (atlas-elisp--excluded-dir-p path)
        (push path files)))
    (setq files (nreverse files))
    (atlas-log :debug "elisp:list-files root=%s total=%d" root (length files))
    files))

(defun atlas-elisp--normalize-paths (root paths)
  "Normalize PATHS (relative or absolute) to a list of absolute .el files under ROOT that exist."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (abs (mapcar (lambda (p)
                        (let ((pp (expand-file-name p root)))
                          (if (file-name-absolute-p p)
                              (expand-file-name p)
                            pp)))
                      paths)))
    (seq-filter (lambda (p)
                  (and (string-match-p "\\.el\\'" p)
                       (file-exists-p p)))
                abs)))

(defun atlas-elisp--rel (root path)
  (string-remove-prefix (file-name-as-directory (expand-file-name root))
                        (expand-file-name path)))

(defun atlas-elisp--file-entry (root path)
  "Build file plist for PATH."
  (let* ((attr (file-attributes path))
         (size (nth 7 attr))
         (mtime (float-time (file-attribute-modification-time attr)))
         (hash (when (and (bound-and-true-p atlas-hash-content)
                          (numberp size)
                          (<= size (or (bound-and-true-p atlas-max-file-size) most-positive-fixnum)))
                 (ignore-errors
                   (with-temp-buffer
                     (insert-file-contents path nil 0 size)
                     (secure-hash 'sha256 (current-buffer)))))))
    (list :path (atlas-elisp--rel root path)
          :size size
          :mtime mtime
          :hash hash
          :lang 'elisp
          :flags (list :generated? nil :vendor? nil))))

(defun atlas-elisp--scan-requires (rel content)
  "Return list of (:type ... :from :to ...) edges for require/provide in CONTENT, bound to REL file."
  (let ((edges nil))
    (condition-case _
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          (condition-case _eof
              (while t
                (let* ((form (read (current-buffer))))
                  (when (and (consp form)
                             (memq (car form) '(provide require)))
                    (let* ((head (car form))
                           (raw (cadr form))
                           (sym (cond
                                 ;; (require 'foo) / (provide 'foo)
                                 ((and (consp raw) (eq (car raw) 'quote) (symbolp (cadr raw))) (cadr raw))
                                 ;; (require foo) — допустимо
                                 ((symbolp raw) raw)
                                 ;; (require "foo") — возьмём basename строки
                                 ((stringp raw) (intern (file-name-base raw)))
                                 (t nil))))
                      (when (symbolp sym)
                        (push (list :type (if (eq head 'provide) 'provide 'require)
                                    :from rel
                                    :to (format "feature:%s" (symbol-name sym))
                                    :weight 1.0 :source 'elisp)
                              edges))))))
            (end-of-file nil)))
      (error nil))
    (nreverse edges)))

(defun atlas-elisp--doc1 (form)
  "Extract first docstring line from FORM if present."
  (let* ((doc (cond
               ;; defun-like: (defun name (args) "doc" ...)
               ((and (listp form) (memq (car form) '(defun defmacro)))
                (let ((maybe (nth 3 form)))
                  (when (stringp maybe) maybe)))
               ;; defvar/defcustom/defconst: often second or third arg is doc
               ((and (listp form) (memq (car form) '(defvar defcustom defconst)))
                (seq-find #'stringp (cdr form)))))
         (first (when (stringp doc)
                  (car (split-string doc "\n" t)))))
    first))

(defun atlas-elisp--symbol-kind (head)
  (pcase head
    ('defun 'function)
    ('defmacro 'macro)
    ('defvar 'var)
    ('defcustom 'custom)
    ('defconst 'const)
    (_ 'symbol)))

(defun atlas-elisp--scan-symbols (rel content)
  "Scan CONTENT for top-level def* forms; return list of symbol plists with REL.
Positions are byte offsets within file contents."
  (let ((symbols nil))
    (condition-case _
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          (let ((eof (make-symbol "eof")))
            (condition-case _eoi
                (while t
                  (let* ((beg (point))
                         (form (read (current-buffer)))
                         (end (point)))
                    (when (and (consp form)
                               (memq (car form) '(defun defmacro defvar defcustom defconst)))
                      (let* ((head (car form))
                             (name (symbol-name (cadr form)))
                             (kind (atlas-elisp--symbol-kind head))
                             (sig (format "%S"
                                          (pcase head
                                            ((or 'defun 'defmacro) (seq-take form 3))
                                            (_ (seq-take form 2)))))
                             (doc1 (atlas-elisp--doc1 form))
                             (id (atlas--symbol-id :lang "elisp" :rel rel
                                                   :name name :beg beg :end end :kind (symbol-name kind))))
                        (push (list :id id :file rel :name name :kind kind
                                    :beg beg :end end :sig sig :doc1 doc1
                                    :exported? nil :source 'elisp :lang 'elisp)
                              symbols)))))
              (end-of-file eof))))
      (error nil))
    (nreverse symbols)))

(cl-defun atlas-elisp-source-run (&key root changed emit done opts)
  "Elisp provider entry point. See atlas-sources for contract.
CHANGED may be :auto, nil, or a list of paths (relative or absolute)."
  (ignore opts)
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (paths (if (and (listp changed) changed)
                    (atlas-elisp--normalize-paths root changed)
                  (atlas-elisp--list-files root))))
    (atlas-log :info "elisp:run root=%s changed=%S paths=%d" root changed (length paths))
    ;; L0 files (inventory) only on full run (no explicit CHANGED list)
    (when (not (and (listp changed) changed))
      (let ((files (mapcar (lambda (p) (atlas-elisp--file-entry root p)) paths)))
        (atlas-log :debug "elisp:L0 emit files=%d" (length files))
        (funcall emit (list (cons :files files)))))
    ;; L1/L2 per file (batch by file to keep memory modest)
    (dolist (path paths)
      (condition-case err
          (let* ((rel (atlas-elisp--rel root path))
                 (content (with-temp-buffer
                            (insert-file-contents path nil 0 atlas-max-file-size)
                            (buffer-substring-no-properties (point-min) (point-max))))
                 (symbols (atlas-elisp--scan-symbols rel content))
                 (edges (atlas-elisp--scan-requires rel content)))
            (atlas-log :trace "elisp:L1/L2 file=%s symbols=%d edges=%d" rel (length symbols) (length edges))
            (funcall emit (list (cons :file rel) (cons :symbols symbols) (cons :edges edges))))
        (error
         (atlas-log :error "elisp:error path=%s err=%S" path err)))))
  (when (functionp done) (funcall done)))

;; Register by default
(atlas-log :info "elisp:provider loading; will register default source")
(require 'atlas-sources)
(atlas-register-source 'elisp
                       :capabilities (list :languages '(elisp)
                                           :kinds '(files symbols edges summaries)
                                           :levels '(L0 L1 L2 L3))
                       :fn #'atlas-elisp-source-run
                       :cost 1.0)

(provide 'atlas-source-elisp)

;;; atlas-source-elisp.el ends here
