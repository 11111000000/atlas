;;; test-atlas-determinism.el --- Deterministic ordering tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas)
(require 'atlas-graph)
(require 'atlas-export)
(require 'json)

(ert-deftest atlas-graph-deterministic-order ()
  "Nodes and edges are returned in deterministic sorted order."
  (let* ((root (make-temp-file "atlas-root-" t)))
    (unwind-protect
        (progn
          (atlas-open root)
          (atlas-import root
                        (list (cons :files
                                    (list (list :path "lisp/a.el" :size 0 :mtime 0.0 :hash nil :lang 'elisp :flags nil)
                                          (list :path "lisp/x.el" :size 0 :mtime 0.0 :hash nil :lang 'elisp :flags nil)))))
          (atlas-import root
                        (list (cons :edges
                                    (list (list :type 'require :from "lisp/a.el" :to "feature:x" :weight 1.0 :source 'elisp)
                                          (list :type 'provide :from "lisp/x.el" :to "feature:x" :weight 1.0 :source 'elisp)))))
          (let* ((g (atlas-graph root "lisp/a.el" :depth 1))
                 (nodes (alist-get :nodes g))
                 (edges (alist-get :edges g)))
            (should (equal nodes (seq-sort #'string< (copy-sequence nodes))))
            (let* ((sorted (seq-sort
                            (lambda (a b)
                              (let* ((ta (format "%s" (plist-get a :type)))
                                     (tb (format "%s" (plist-get b :type)))
                                     (fa (format "%s" (plist-get a :from)))
                                     (fb (format "%s" (plist-get b :from)))
                                     (oa (format "%s" (plist-get a :to)))
                                     (ob (format "%s" (plist-get b :to))))
                                (or (string-lessp ta tb)
                                    (and (string= ta tb)
                                         (or (string-lessp fa fb)
                                             (and (string= fa fb)
                                                  (string-lessp oa ob)))))))
                            (copy-sequence edges))))
              (should (equal edges sorted))))))
    (ignore-errors (delete-directory root t))))

(ert-deftest atlas-export-llm-deterministic ()
  "LLM JSON pack has deterministic ordering in graph nodes and top/files/imports."
  (let* ((root (make-temp-file "atlas-root-" t))
         (jsonf (expand-file-name "pack.json" root)))
    (unwind-protect
        (progn
          (atlas-open root)
          (atlas-import root
                        (list (cons :symbols
                                    (list (list :id "elisp:lisp/a.el#foo@10-20/function"
                                                :file "lisp/a.el" :name "foo" :kind 'function
                                                :beg 10 :end 20 :sig "(defun foo ())" :doc1 "doc" :source 'elisp :lang 'elisp)
                                          (list :id "elisp:lisp/x.el#bar@10-20/function"
                                                :file "lisp/x.el" :name "bar" :kind 'function
                                                :beg 10 :end 20 :sig "(defun bar ())" :doc1 "doc" :source 'elisp :lang 'elisp)))))
          (atlas-export-llm root "f" :k 2 :graph-depth 1 :path jsonf)
          (with-temp-buffer
            (insert-file-contents jsonf)
            (let* ((obj (json-parse-buffer :object-type 'alist))
                   (nodes (alist-get 'nodes (alist-get 'graph obj)))
                   (top   (alist-get 'top obj))
                   (files (alist-get 'files obj))
                   (imports (alist-get 'imports obj)))
              (should (equal (append nodes nil)
                             (seq-sort #'string< (append nodes nil))))
              (let* ((top-ids (mapcar (lambda (o) (alist-get 'id o)) (append top nil))))
                (should (equal top-ids (seq-sort #'string< (copy-sequence top-ids)))))
              (should (equal (append files nil)
                             (seq-sort #'string< (append files nil))))
              (should (equal (append imports nil)
                             (seq-sort #'string< (append imports nil)))))))
      (ignore-errors (delete-directory root t)))))

(provide 'test-atlas-determinism)
