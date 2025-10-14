(
 (:type require :from "lisp/foo.el" :to "feature:foo-core" :weight 1.0 :source elisp)
 (:type provide :from "lisp/foo-core.el" :to "feature:foo-core" :weight 1.0 :source elisp))