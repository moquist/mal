(ns step0-repl
  (:require utils))

(defn READ [x] x)
(defn EVAL [x] x)
(defn PRINT [x] x)
(defn rep [x]
  (-> x
      READ
      EVAL
      PRINT))

(defn prompt []
  (print "user> ")
  (flush)
  (read-line))

(defn -main []
  (if-let [x (prompt)]
    (do
      (println (rep x))
      (recur))))
