;; v1 symbols.schema.sexp (Normative)
(:entity Symbol
 :keys (:id string :file string :name string :kind symbol
        :beg integer :end integer
        :sig (or string nil) :doc1 (or string nil)
        :exported? (or boolean nil)
        :source symbol :lang symbol)
 :constraints ((:id :required t)
               (:file :required t)
               (:name :required t)
               (:kind :required t)
               (:beg :required t)
               (:end :required t)))