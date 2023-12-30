(ns step2-eval
  (:require [clojure.string :as str]
            utils
            types
            reader
            printer))

;; ========================================
;; REPL Environment

(def built-in-env [['+ clojure.core/+]
                   ['- clojure.core/-]
                   ['* clojure.core/*]
                   ['/ clojure.core//]])

(defn gen-env [env]
  (reduce (fn [r [f-sym f]]
            (assoc r
                  (types/->MalDatum :symbol f-sym)
                  (types/->MalDatum :fn f)))
          {}
          env))

(def env (gen-env built-in-env))

;; ========================================
;; EVAL
(declare eval-ast)
(defn EVAL [x env]
  (prn :moquist-EVAL :x x)
  (cond
    (-> x :typ (not= :list))
    (eval-ast x env)

    (-> x :datum-val empty?)
    (types/->MalDatum :list [])

    :else
    (let [[f & args] (:datum-val (eval-ast x env))]
          (prn :moquist-EVAL-apply :f f :args args)
          (types/->MalDatum :undetermined
                            (apply (:datum-val f) (map :datum-val args))))
    ))

(defn eval-ast [ast env]
  (condp = (:typ ast)
    :symbol (or
              (get env ast)
              (throw (ex-info (format "Unable to resolve symbol: %s in this context"
                                      (:datum-val ast))
                              {:cause :undefined-symbol
                               :env env
                               :symbol ast})))
    :list (->> ast :datum-val (mapv #(EVAL % env)) (types/->MalDatum :list))
    :vector (->> ast :datum-val (mapv #(EVAL % env)) (types/->MalDatum :vector))
    :map (->> ast
              :datum-val
              (map (fn [[k v]]
                     [(EVAL k env)
                      (EVAL v env)]))
              (into {})
              (types/->MalDatum :map))
    ast))

;; ========================================
;; REPL

(defn READ [x]
  (cond
    (string? x) (reader/mal-read-string x)
    (satisfies? reader/MalRead x) (reader/read-form x)
    :else (throw (Exception. (format "READ with invalid input of type %s" (type x))))))

(defn PRINT [form]
  (prn :moquist-PRINT form)
  (when (satisfies? printer/MalPrinter form)
    (printer/mal-print-string form true)))

(defn rep [x]
  (let [[reader form] (READ x)]
    [reader (-> form (EVAL env) PRINT)]))


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
    (try
      (loop [[reader result] (rep input)]
        (when result (println result))
        (when (and reader (not= :reader/peeked-into-the-abyss (reader/mal-peek reader)))
          (recur (rep reader))))
      (catch clojure.lang.ExceptionInfo e
        (binding [*out* *err*]
          (prn e))))
    (recur)))
