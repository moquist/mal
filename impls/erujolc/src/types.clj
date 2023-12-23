(ns types
  (:require [printer :refer [MalPrinter mal-print-list mal-print-string]]))

;; TODO: Use 'mal-types to validate on read, general error checking, introspection
(def mal-types #{:list :bool :vector :string :keyword :map :nil :int :fn})

(defrecord MalDatum [typ datum-val]
  MalPrinter
  (mal-print-string [this print-readably]
    (prn ::MalDatum-MalPrinter :typ typ :datum-val datum-val)
    (condp = typ
      :fn "#<function>"
      :keyword datum-val
      :list (str "(" (mal-print-list this print-readably) ")")
      :vector (str "[" (mal-print-list this print-readably) "]")
      :map (str "{" (mal-print-list this print-readably) "}")
      :string ((if print-readably pr-str print-str) datum-val)
      :nil "nil"
      (str datum-val)))
  (mal-print-list [_ print-readably]
    (->> datum-val
         (map #(mal-print-string % print-readably))
         (interpose " ")
         (apply str))))
