(ns step2-eval
  (:require [clojure.string :as str]
            utils
            types
            reader
            printer))

;; ========================================
;; REPL Environment

(defn- gen-env-entry [f-sym f-type]
  `[(types/->MalDatum :symbol ~f-sym)
    (types/->MalDatum :fn (fn [& args#]
                            ;; holy cow... functions are typed!
                            ;; I need "types" for collections, though "type" is probably not the best-chosen word
                            ;; I shouldn't need types for primitives
                            (types/->MalDatum ~f-type
                                              (apply ~(resolve f-sym) args#))))])

(def env-symbols [['+ :int]
                  ['- :int]
                  ['* :int]
                  ['/ :int]])

(comment
  (apply gen-env-entry ['+ :int])

  )

(defmacro def-env []
  `(def env ~(->> env-symbols
                  (mapcat #(apply gen-env-entry %))
                  (apply hash-map))))
(macroexpand (def-env))

;; ========================================
;; EVAL
(declare eval-ast)
(defn EVAL [x env]
  (utils/debug :EVAL :x x :env env)
  (let [evaluated (eval-ast x env)]
    (utils/debug :EVAL :evaluated evaluated)
    (if (= :list (:type x))
      (let [[f & args] (->> evaluated
                            :val
                            (map :val))]
        (utils/debug :EVAL :f f :args args)
        (apply f args))
      evaluated)))

(defn eval-ast [ast env]
  (utils/debug :eval-ast :ast ast :env env)
  (condp = (:type ast)
    :symbol (env ast)
    ;; TODO: DRY!
    :list (assoc ast :val (map #(EVAL % env) (:val ast)))
    :vector (assoc ast :val (map #(EVAL % env) (:val ast)))
    :map (assoc ast :val (map #(EVAL % env) (:val ast)))
    ast))

;; ========================================
;; REPL
(defn READ [x] (reader/mal-read-string x))
(defn PRINT [x] (printer/mal-print-string x true))
(defn rep [x]
  (-> x
      READ
      second ; throw away the MalReader, keep what was read
      (EVAL env)
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
        (catch Exception e (ex-data e)))
      (recur))))
