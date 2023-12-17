(ns step0-repl
  (:require [clojure.string :as str]))

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
  (some-> (read-line) str/trim))

(defn -main []
  (when-let [x (prompt)]
    (println (rep x))
    (recur)))

(comment
  (-main)
  (READ 7)
  (rep 7)

  (def yef (prompt))
  (prn yef)
  (def mef (read-line))
  (do (prn mef)
      (type mef)
      (count mef))
  )
