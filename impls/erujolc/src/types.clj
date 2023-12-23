(ns types
  (:require [printer]))

;; TODO: Use 'mal-types to validate on read, general error checking, introspection
(def mal-types #{:list :bool :vector :string :keyword :map :nil :int :fn})

(defrecord MalDatum [typ datum-val]
  printer/MalPrinter
  (printer/-mal-print-string [this print-readably]
    #_
    (prn ::MalDatum-MalPrinter :typ typ :datum-val datum-val)
    (condp = typ
      :fn "#<function>"
      :list (str "(" (printer/-mal-print-list this print-readably) ")")
      :map (str "{" (printer/-mal-print-map this print-readably) "}")
      :string ((if print-readably pr-str print-str) datum-val)
      :vector (str "[" (printer/-mal-print-list this print-readably) "]")
      :nil "nil"
      (str datum-val)))
  (printer/-mal-print-map [_ print-readably]
    (->> datum-val
         (mapcat (fn [[k v]]
                   [(printer/-mal-print-string k print-readably)
                    (printer/-mal-print-string v print-readably)]))
         (interpose " ")
         (apply str))
    )
  (printer/-mal-print-list [_ print-readably]
    (->> datum-val
         (map #(printer/-mal-print-string % print-readably))
         (interpose " ")
         (apply str))))
