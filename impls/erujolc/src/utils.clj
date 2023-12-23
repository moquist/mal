(ns utils)

(def debug? true)

(defn debug [& stuff]
  (when debug? (apply prn stuff)))
