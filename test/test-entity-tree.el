;;; test-entity-tree.el --- ERT tests for atlas-entity-tree -*- lexical-binding: t; -*-

(require 'ert)
(require 'atlas-events)
(require 'atlas-entity-tree)

(ert-deftest atlas-entity-tree-open-buffer ()
  "Opening entity tree creates buffer with correct mode."
  (let ((buf (atlas-entity-tree default-directory)))
    (should (buffer-live-p buf))
    (with-current-buffer buf
      (should (derived-mode-p 'atlas-entity-tree-mode))
      (should (string-match-p "Atlas Entities" (buffer-string))))))

(ert-deftest atlas-entity-tree-reacts-to-events ()
  "Tree reflects indexing status on events and clears it on done."
  (let ((buf (atlas-entity-tree default-directory)))
    (should (buffer-live-p buf))
    (atlas-events-publish :atlas-index-start :root default-directory :full t)
    (with-current-buffer buf
      (should (string-match-p "indexing" (buffer-string))))
    (atlas-events-publish :atlas-index-progress :files 1 :symbols 2 :edges 3)
    (with-current-buffer buf
      (should (string-match-p "f=1 s=2 e=3" (buffer-string))))
    (atlas-events-publish :atlas-index-done :root default-directory)
    (with-current-buffer buf
      (should-not (string-match-p "indexing" (buffer-string))))))

(provide 'test-entity-tree)
