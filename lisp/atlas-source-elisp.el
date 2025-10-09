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

(declare-function atlas-root-dir "atlas" (root))
(declare-function atlas--symbol-id "atlas" (&key lang rel name beg end kind))

(defgroup atlas-elisp nil
  "Elisp source for Atlas."
  :group 'atlas
  :prefix "atlas-elisp-")

(defun atlas-elisp--excluded-dir-p (name)
  (seq-some (lambda (re) (string-match-p re name))
            (bound-and-true-p atlas-exclude-dirs)))

(defun atlas-elisp--list-files (root)
  "List .el files under ROOT respecting excludes."
  (let ((files nil))
    (dolist (path (directory-files-recursively root "\\.el\\'" nil))
      (unless (atlas-elisp--excluded-dir-p path)
        (push path files)))
    (nreverse files)))

(defun atlas-elisp--rel (root path)
  (string-remove-prefix (file-name-as-directory (expand-file-name root))
                        (expand-file-name path)))

(defun atlas-elisp--file-entry (root path)
  "Build file plist for PATH."
  (let* ((attr (file-attributes path))
         (size (nth 7 attr))
         (mtime (float-time (file-attribute-modification-time attr))))
    (list :path (atlas-elisp--rel root path)
          :size size
          :mtime mtime
          :hash nil
          :lang 'elisp
          :flags (list :generated? nil :vendor? nil))))

(defun atlas-elisp--scan-requires (content)
  "Return list of (:type ... :from :to ...) edges for require/provide in CONTENT."
  (let ((edges nil))
    (save-match-data
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "(provide\\s-+'\\([^)\\s-]+\\))" nil t)
          (push (list :type 'provide :from (format "feature:%s" (match-string 1))
                      :to nil :weight 1.0 :source 'elisp) edges))
        (goto-char (point-min))
        (while (re-search-forward "(require\\s-+'\\([^)\\s-]+\\))" nil t)
          (push (list :type 'require :from nil
                      :to (format "feature:%s" (match-string 1))
                      :weight 1.0 :source 'elisp) edges))))
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
  "Scan CONTENT for top-level def* forms; return list of symbol plists with REL."
  (let ((symbols nil))
    (condition-case _
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          (let ((eof (make-symbol "eof")))
            (condition-case _eoi
                (while t
                  (let ((form (read (current-buffer))))
                    (when (and (consp form)
                               (memq (car form) '(defun defmacro defvar defcustom defconst)))
                      (let* ((head (car form))
                             (name (symbol-name (cadr form)))
                             (kind (atlas-elisp--symbol-kind head))
                             (sig (format "%S" (pcase head
                                                 ((or 'defun 'defmacro) (seq-take form 3))
                                                 (_ (seq-take form 2)))))
                             (doc1 (atlas-elisp--doc1 form))
                             (beg 0) (end 0)
                             (id (atlas--symbol-id :lang "elisp" :rel rel
                                                   :name name :beg beg :end end :kind (symbol-name kind))))
                        (push (list :id id :file rel :name name :kind kind
                                    :beg beg :end end :sig sig :doc1 doc1
                                    :exported? nil :source 'elisp)
                              symbols)))))
              (end-of-file eof))))
      (error nil))
    (nreverse symbols)))

(cl-defun atlas-elisp-source-run (&key root changed emit done opts)
  "Elisp provider entry point. See atlas-sources for contract."
  (ignore changed opts)
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (paths (atlas-elisp--list-files root)))
    ;; L0 files
    (let ((files (mapcar (lambda (p) (atlas-elisp--file-entry root p)) paths)))
      (funcall emit (list :files files)))
    ;; L1/L2 per file (batch by file to keep memory modest)
    (dolist (path paths)
      (condition-case err
          (let* ((rel (atlas-elisp--rel root path))
                 (content (with-temp-buffer
                            (insert-file-contents path nil 0 atlas-max-file-size)
                            (buffer-substring-no-properties (point-min) (point-max))))
                 (symbols (atlas-elisp--scan-symbols rel content))
                 (edges (atlas-elisp--scan-requires content)))
            (funcall emit (list :symbols symbols :edges edges)))
        (error (message "atlas-elisp-source error on %s: %S" path err)))))
  (when (functionp done) (funcall done)))

;; Register by default
(require 'atlas-sources)
(atlas-register-source 'elisp
                       :capabilities (list :languages '(elisp)
                                           :kinds '(files symbols edges summaries)
                                           :levels '(L0 L1 L2 L3))
                       :fn #'atlas-elisp-source-run
                       :cost 1.0)

(provide 'atlas-source-elisp)

;;; atlas-source-elisp.el ends here
