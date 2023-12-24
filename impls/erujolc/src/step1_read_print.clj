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
  (let [y (try (reader/mal-read-string x)
               (catch clojure.lang.ExceptionInfo e
                 (binding [*out* *err*]
                   (prn e))))]
    (utils/debug ::READ :y y)
    y
    ))
(defn EVAL [x] x)
(defn PRINT [form]
  (when form
    (printer/mal-print-string form true)))
(defn rep [x]
  #_
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
  (when-let [x (prompt)]
    (try
      (some-> (rep x) println)
      ;; TODO: watch for known exceptions, rather than eating them all. :)
      (catch Exception e
        (throw e)
        #_
        (println (.getMessage e))))
    (recur)))
