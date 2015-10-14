(ns utils)

(def debug? false)

(defn debug [& stuff]
  (when debug? (apply println stuff)))
