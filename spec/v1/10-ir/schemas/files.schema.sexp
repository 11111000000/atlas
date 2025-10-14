;; v1 files.schema.sexp (Normative)
(:entity File
 :keys (:path string :size integer :mtime float :hash (or string nil)
        :lang symbol
        :flags (:generated? boolean :vendor? boolean))
 :constraints ((:path :required t)
               (:lang :required t)))