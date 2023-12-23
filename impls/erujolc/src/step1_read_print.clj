(ns step1-read-print
  (:require [clojure.string :as str]
            utils
            types
            reader
            printer))

;; ========================================
;; REPL
(defn READ [x]
  ;; moquist TODO: the reader can't read all the forms -- must REPL in between forms so you can, e.g., (def y 7) (+ y 3)
  (let [y (reader/mal-read-string x)]
    (utils/debug ::READ :y y)
    y
    ))
(defn EVAL [x] x)
(defn PRINT [forms]
  (assert forms)
  (prn ::PRINT :moquist-forms forms)
  (doall
    (for [form forms]
      (printer/mal-print-string form true))))
(defn rep [x]
  (prn ::rep :x x)
  (-> x
      READ
      second ; throw away the MalReader, keep what was read
      EVAL
      PRINT))

(defn prompt
  "Print a prompt, read a line.

  Return the input line."
  []
  (print "user> ")
  (flush)
  (some-> (read-line) str/trim))

(defn -main
  "Prompt for input, process the input with READ-EVAL-PRINT, and recur."
  []
  (if-let [x (prompt)]
    (do
      (prn ::-main :type-x (type x) :moquist-x x)
      (try
        (println (rep x))
        ;; TODO: watch for known exceptions, rather than eating them all. :)
        (catch Exception e
          (throw e)
          #_
          (println (.getMessage e))))
      (recur))))
