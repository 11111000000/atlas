;; v1 facts.schema.sexp (Normative)
(:entity Fact
 :keys (:id string :subject any :predicate symbol :object any
        :source symbol :confidence float
        :weight (or float nil)
        :evidence (or list nil)
        :ts float :ttl (or integer nil) :expires (or float nil)
        :author (or string symbol nil) :version (or string nil)
        :tags (or list nil))
 :constraints ((:subject :required t)
               (:predicate :required t)
               (:source :required t)
               (:confidence :required t)))