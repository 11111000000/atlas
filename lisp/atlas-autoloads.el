;;; atlas-autoloads.el --- Autoloads for Atlas dev tree -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this file to get autoloads when working from source (no package.el).
;; Example:
;;   (add-to-list 'load-path "/home/az/Code/atlas/lisp")
;;   (require 'atlas-autoloads)
;;   (atlas-progress-mode 1)

;;; Code:

;; UI
(autoload 'atlas-progress-mode "atlas-ui" nil t)

;; Entity tree
(autoload 'atlas-entity-tree "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-refresh "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-open-at-point "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-peek-at-point "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-copy-at-point "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-actions "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-toggle-follow "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-search "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-edges "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-plan "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-search-command "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-edges-command "atlas-entity-tree" nil t)
(autoload 'atlas-entity-tree-plan-command "atlas-entity-tree" nil t)
(autoload 'atlas-entities "atlas-entity-tree" nil t)

;; Explorer
(autoload 'atlas-explore "atlas-explore" nil t)
(autoload 'atlas-explore-open-at-point "atlas-explore" nil t)

;; Watch
(autoload 'atlas-watch-mode "atlas-watch" nil t)
(autoload 'atlas-watch-add-root "atlas-watch" nil t)
(autoload 'atlas-watch-remove-root "atlas-watch" nil t)
(autoload 'atlas-watch-list-roots "atlas-watch" nil t)

;; Core and commands
(autoload 'atlas-open "atlas" nil t)
(autoload 'atlas-close "atlas" nil t)
(autoload 'atlas-index "atlas" nil t)
(autoload 'atlas-reindex-changed "atlas" nil t)
(autoload 'atlas-stats "atlas" nil t)
(autoload 'atlas-query-command "atlas" nil t)

;; Index helpers
(autoload 'atlas-update "atlas-index" nil t)

;; Export commands
(autoload 'atlas-graph-export-command "atlas-export" nil t)
(autoload 'atlas-export-llm-command "atlas-export" nil t)

;; Log
(autoload 'atlas-log-open "atlas-log" nil t)
(autoload 'atlas-log-clear "atlas-log" nil t)

(provide 'atlas-autoloads)

;;; atlas-autoloads.el ends here
