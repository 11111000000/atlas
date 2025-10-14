;; v1 meta.schema.sexp (Normative)
(:entity Meta
 :keys (:schema integer
        :project-root string
        :generated-at float
        :counts (:files integer :symbols integer :edges integer :facts integer :summaries integer)
        :languages list
        :opts (:segment-threshold integer :compressed? boolean))
 :constraints ((:schema :required t)
               (:project-root :required t)
               (:generated-at :required t)
               (:counts :required t)))
