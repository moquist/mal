(ns types
  (:require [printer]))

;; TODO: Use 'mal-types to validate on read, general error checking, introspection
;; difference btw static and dynamic typing is: dynamic typing allows any type to be assigned to any variable without the programmer declaring that at compile time
(def mal-types
  "Mal needs to know all these types in order to PRINT them..."
  #{:atom
    :list
    :exception
    :vector
    :bool
    :string
    :keyword
    :map
    :nil
    :host-fn ; implemented in the host language, built-in
    :core-fn ; implemented in the host language, built-in
    :fn* ; implemented in mal
    :int
    :type
    ;; figured out on-the-fly, as necessary
    :undetermined})

(defrecord MalDatum [typ datum-val]
  printer/MalPrinter
  (printer/-mal-print-string [this print-readably]
    (condp = typ
      :host-fn "#<host-function>"
      :core-fn "#<core-function>"
      :exception (printer/mal-print-string datum-val print-readably)
      :atom (str "(atom "
                 (printer/mal-print-string
                   (-> this :meta-datum :mal-atoms deref (get datum-val))
                   print-readably)
                 ")")
      :fn* "#<function>"
      :list (str "(" (printer/-mal-print-list this print-readably) ")")
      :map (str "{" (printer/-mal-print-map this print-readably) "}")
      :string (do #_(prn :moquist-string print-readably datum-val) ((if print-readably pr-str print-str) datum-val))
      :vector (str "[" (printer/-mal-print-list this print-readably) "]")
      :nil "nil"
      (str datum-val)))
  (printer/-mal-print-map [_ print-readably]
    (->> datum-val
         (mapcat (fn [[k v]]
                   [(printer/-mal-print-string k print-readably)
                    (printer/-mal-print-string v print-readably)]))
         (interpose " ")
         (apply str)))
  (printer/-mal-print-list [_ print-readably]
    (->> datum-val
         (map #(printer/-mal-print-string % print-readably))
         (interpose " ")
         (apply str))))

(defn mal-datum [typ datum-val]
  (->MalDatum typ datum-val))

(def mal-nil (->MalDatum :nil nil))
(def mal-true (->MalDatum :bool true))
(def mal-false (->MalDatum :bool false))

(defmethod clojure.core/print-method ::fn* [x ^java.io.Writer w]
  (.write w "a fn")
  #_
  (.write w (str (select-keys x [:ast :binds]))))


(extend-protocol printer/MalPrinter
  Object
  (printer/-mal-print-string [this print-readably]
    (prn (Exception. "where did this come from?"))
    (str "Erujolc bug: unprintable " this " of type " (type this))))
