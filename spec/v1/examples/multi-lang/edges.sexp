(
 ;; Lisp feature deps
 (:type require :from "lisp/atlas.el" :to "feature:atlas-store" :source elisp)
 (:type provide :from "lisp/atlas.el" :to "feature:atlas" :source elisp)
 ;; TS imports
 (:type import :from "src/app/main.ts" :to "module:@angular/core" :source treesit)
 (:type export :from "src/app/main.ts" :to "symbol:app.init" :source treesit)
 ;; Haskell imports
 (:type import :from "app/Main.hs" :to "module:Data.Text" :source heuristic)
 ;; Python imports
 (:type import :from "src/cli.py" :to "module:argparse" :source treesit)
 ;; Type references (TS/Hs)
 (:type type-ref :from "ts:src/app/main.ts#init@120-340/function" :to "interface:Config" :source lsp)
 ;; Haskell instance
 (:type instance-of :from "module:My.Type.Instance" :to "typeclass:MyClass" :source lsp))