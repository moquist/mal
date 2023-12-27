(ns step1-read-print
  (:require [clojure.string :as str]
            utils
            types
            reader
            printer))

;; ========================================
;; REPL
(defn READ [x]
  (try (reader/mal-read-string x)
       (catch clojure.lang.ExceptionInfo e
         (binding [*out* *err*]
           (prn e)))))

(defn EVAL [x] x)

(defn PRINT [form]
  (prn :moquist-form1 form)
  (when (satisfies? printer/MalPrinter form)
    (printer/mal-print-string form true)))

(defn rep [x]
  (let [[reader form] (READ x)]
    [reader (-> form EVAL PRINT)]))

(defn rep2 [reader]
  (let [[reader form] (reader/read-form reader)]
    [reader (-> form EVAL PRINT)]))

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
  (when-let [input (prompt)]
    (loop [[reader result] (rep input)]
      (when result (println result))
      (when (and reader (not= :reader/peeked-into-the-abyss (reader/mal-peek reader)))
        (recur (rep2 reader))))
    (recur)))
