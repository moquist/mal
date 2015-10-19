(ns types
  (:require printer))

;; TODO: Use 'mal-types to validate on read, general error checking, introspection
(def mal-types #{:list :vector :string :keyword :map :nil :int :fn})

(defrecord MalDatum [type val]
  printer/MalPrinter
  (mal-print-string [this print-readably]
    (condp = type
      :fn "#<function>"
      :keyword val
      :list (str "(" (printer/mal-print-list this) ")")
      :vector (str "[" (printer/mal-print-list this) "]")
      :map (str "{" (printer/mal-print-list this) "}")
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
         (map #(printer/mal-print-string % print-readably))
         (interpose " ")
         (apply str))))
