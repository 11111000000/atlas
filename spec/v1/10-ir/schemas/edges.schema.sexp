;; v1 edges.schema.sexp (Normative)
(:entity Edge
 :keys (:type (or symbol string) :from any :to any
        :weight (or float nil)
        :source symbol
        :span (or (cons integer integer) nil))
 :constraints ((:type :required t)
               (:from :required t)
               (:to :required t)))