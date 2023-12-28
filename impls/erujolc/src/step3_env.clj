(ns step3-env
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
  (cond
    (-> x :typ (not= :list))
    (eval-ast x env)

    (-> x :datum-val empty?)
    '()

    :else
    (let [[f & args] (eval-ast x env)]
      (apply f args))))

(defn eval-ast [ast env]
  (condp = (:typ ast)
    :symbol (if-let [x (env ast)]
              (:datum-val x)
              (throw (ex-info (format "Unable to resolve symbol: %s in this context"
                                      (:datum-val ast))
                              {:cause :undefined-symbol
                               :env env
                               :symbol ast})))
    :list (->> ast :datum-val (map #(EVAL % env)))
    :vector (->> ast :datum-val (mapv #(EVAL % env)))
    :map (->> ast
              :datum-val
              (map (fn [[k v]]
                     [(EVAL k env)
                      (EVAL v env)]))
              (into {}))
    (:datum-val ast)))

(defn wrapped-EVAL [x env]
  (types/->MalDatum :undetermined
                    (EVAL x env)))

;; ========================================
;; REPL

(defn READ [x]
  (cond
    (string? x) (reader/mal-read-string x)
    (satisfies? reader/MalRead x) (reader/read-form x)
    :else (throw (Exception. (format "READ with invalid input of type %s" (type x))))))

(defn PRINT [form]
  (when (satisfies? printer/MalPrinter form)
    (printer/mal-print-string form true)))

(defn rep [x]
  (let [[reader form] (READ x)]
    [reader (-> form (wrapped-EVAL env) PRINT)]))

(defn LOOP
  "Loop through the forms in the provided input"
  [input]
  (try
    (loop [[reader result] (rep input)]
      (when result (println result))
      (when (and reader (not= :reader/peeked-into-the-abyss (reader/mal-peek reader)))
        (recur (rep reader))))
    (catch clojure.lang.ExceptionInfo e
      (binding [*out* *err*]
        (prn e)))))

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
    (LOOP input)
    (recur)))
