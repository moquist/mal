(ns step1-read-print
  (:require utils
            types
            reader
            printer))

;; ========================================
;; REPL
(defn READ [x] (reader/mal-read-string x))
(defn EVAL [x] x)
(defn PRINT [x] (printer/mal-print-string x true))
(defn rep [x]
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
  (read-line))

(defn -main
  "Prompt for input, process the input with READ-EVAL-PRINT, and recur."
  []
  (if-let [x (prompt)]
    (do
      (try
        (println (rep x))
        ;; TODO: watch for known exceptions, rather than eating them all. :)
        (catch Exception _))
      (recur))))
