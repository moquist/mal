(ns types
  (:require printer))

;; TODO: Use 'mal-types to validate on read, general error checking, introspection
(def mal-types #{:list :vector :string :keyword :map :nil :int :fn})

(defrecord MalDatum [type val]
  printer/MalPrinter
  (mal-print-string [this]
    (condp = type
      :fn "#<function>"
      :keyword val
      :list (str "(" (printer/mal-print-list this) ")")
      :vector (str "[" (printer/mal-print-list this) "]")
      :map (str "{" (printer/mal-print-list this) "}")
      :string (str \" (-> val
                          ;; Replace escaped quotes with println-happy escaped quotes.
                          (clojure.string/replace #"\"" "\\\\\\\"")
                          ;; Replace newlines with println-happy escaped newlines.
                          (clojure.string/replace #"\n" "\\\\\\n"))
                   \")
      :nil "nil"
      (str val)))
  (mal-print-list [_]
    (->> val
         (map printer/mal-print-string)
         (interpose " ")
         (apply str))))
