(ns utils)

(def debug? false)

(defn debug [& stuff]
  (when debug? (apply prn stuff)))

(defmacro defprotocol-once [sym & forms]
  (when-not (ns-resolve *ns* sym)
    `(defprotocol ~sym ~@forms)))
