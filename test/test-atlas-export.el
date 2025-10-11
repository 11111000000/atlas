;;; test-atlas-export.el --- ERT tests for atlas graph/LLM exports -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'atlas)
(require 'atlas-dump)
(require 'atlas-graph)
(require 'atlas-export)

(ert-deftest atlas-graph-export-dot-basic ()
  "Export a tiny graph to DOT and validate content."
  (let* ((root (make-temp-file "atlas-root-" t))
         (dot (expand-file-name "g.dot" root)))
    (unwind-protect
        (progn
          (atlas-open root)
          ;; Minimal inventory
          (atlas-import root
                        (list (cons :files
                                    (list (list :path "lisp/a.el" :size 0 :mtime 0.0 :hash nil :lang 'elisp :flags nil)
                                          (list :path "lisp/x.el" :size 0 :mtime 0.0 :hash nil :lang 'elisp :flags nil)))))
          ;; Edges: a.el requires feature:x, x.el provides feature:x
          (atlas-import root
                        (list (cons :edges
                                    (list (list :type 'require :from "lisp/a.el" :to "feature:x" :weight 1.0 :source 'elisp)
                                          (list :type 'provide :from "lisp/x.el" :to "feature:x" :weight 1.0 :source 'elisp)))))
          (atlas-graph-export root "lisp/a.el" :format 'dot :path dot :depth 1)
          (should (file-exists-p dot))
          (with-temp-buffer
            (insert-file-contents dot)
            (let ((s (buffer-string)))
              (should (string-match-p "lisp/a\\.el" s))
              (should (string-match-p "feature:x" s))
              (should (string-match-p "lisp/x\\.el" s)))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest atlas-export-llm-basic ()
  "Export LLM JSON pack and validate key shape."
  (let* ((root (make-temp-file "atlas-root-" t))
         (jsonf (expand-file-name "pack.json" root)))
    (unwind-protect
        (progn
          (atlas-open root)
          (atlas-import root
                        (list (cons :files
                                    (list (list :path "lisp/a.el" :size 0 :mtime 0.0 :hash nil :lang 'elisp :flags nil)))))
          (atlas-import root
                        (list (cons :symbols
                                    (list (list :id "elisp:lisp/a.el#foo@10-20/function"
                                                :file "lisp/a.el" :name "foo" :kind 'function
                                                :beg 10 :end 20 :sig "(defun foo (x))" :doc1 "doc" :source 'elisp :lang 'elisp)))))
          (atlas-export-llm root "foo" :k 1 :budget 200 :graph-depth 1 :path jsonf)
          (should (file-exists-p jsonf))
          (with-temp-buffer
            (insert-file-contents jsonf)
            (goto-char (point-min))
            (let ((obj (json-parse-buffer :object-type 'alist)))
              (should (equal (alist-get 'query obj) "foo"))
              (should (vectorp (alist-get 'top obj)))
              (should (vectorp (alist-get 'files obj)))
              (should (alist-get 'graph obj)))))
      (ignore-errors (delete-directory root t)))))
