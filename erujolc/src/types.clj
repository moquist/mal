(ns types
  (:require [printer :refer [MalPrinter mal-print-list mal-print-string]]))

;; TODO: Use 'mal-types to validate on read, general error checking, introspection
(def mal-types #{:list :vector :string :keyword :map :nil :int :fn})

(defrecord MalDatum [type val]
  MalPrinter
  (mal-print-string [this print-readably]
    (condp = type
      :fn "#<function>"
      :keyword val
      :list (str "(" (mal-print-list this print-readably) ")")
      :vector (str "[" (mal-print-list this print-readably) "]")
      :map (str "{" (mal-print-list this print-readably) "}")
      :string (str \" (if print-readably
                        (-> val
                            ;; Replace escaped quotes with println-happy escaped quotes.
                            (clojure.string/replace #"\"" "\\\\\\\"")
                            ;; Replace newlines with println-happy escaped newlines.
                            (clojure.string/replace #"\n" "\\\\\\n"))
                        val)
                   \")
      :nil "nil"
      (str val)))
  (mal-print-list [_ print-readably]
    (->> val
         (map #(mal-print-string % print-readably))
         (interpose " ")
         (apply str))))
