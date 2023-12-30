(ns step3-env
  (:require [clojure.string :as str]
            env
            printer
            reader
            types
            utils))

;; ========================================
;; REPL Environment
(declare eval-ast)

(def built-in-env [['+ clojure.core/+]
                   ['- clojure.core/-]
                   ['* clojure.core/*]
                   ['/ clojure.core//]])

(defn mal-def!
  "Set k to the evaluated form in env, returning env"
  [env k form]
  (prn :moquist-mal-def! :k k :form form :ea (eval-ast form env))
  (let [v (types/->MalDatum :undetermined (eval-ast form env))]
    (env/set env k v)
    v))

(def mal-specials
  {(types/->MalDatum :symbol 'def!) mal-def!})

;; ========================================
;; EVAL
(defn EVAL [x env]
  (cond
    (-> x :typ (not= :list))
    (eval-ast x env)

    (-> x :datum-val empty?)
    '()

    :else
    (let [[f & args] (:datum-val x)]
      (condp = f
        (types/->MalDatum :symbol 'def!)
        (apply mal-def! env args)

        (types/->MalDatum :symbol 'let*)
        ;; TODO: allow more than one form
        (let [[bindings form] args
              _ (when (-> bindings count odd?)
                  (throw (ex-info "odd number of forms in binding vector"
                                  {:cause :invalid-binding-vector
                                   :bindings bindings})))
              env2 (reduce (fn [r [k v]]
                             (env/set r k (types/->MalDatum :undetermined (eval-ast v r))))
                           (env/mal-environer env)
                           bindings)]
          (types/->MalDatum :undetermined (eval-ast form env2)))

        (let [[f & args] (eval-ast x env)]
          (apply f args))))))

(defn eval-ast [ast env]
  (condp = (:typ ast)
    :symbol (if-let [x (env/get env ast)]
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

(defn rep [x env]
  (let [[reader form] (READ x)]
    [reader (-> form (wrapped-EVAL env) PRINT)]))

(defn LOOP
  "Loop through the forms in the provided input"
  [input env]
  (try
    (loop [[reader result] (rep input env)]
      (when result (println result))
      (when (and reader (not= :reader/peeked-into-the-abyss (reader/mal-peek reader)))
        (recur (rep reader env))))
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
  (let [env (reduce (fn [r [sym f]]
                      (env/set r
                               (types/->MalDatum :symbol sym)
                               (types/->MalDatum :fn f)))
                    (env/mal-environer nil)
                    built-in-env)]
    (loop []
      (when-let [input (prompt)]
        (LOOP input env)
        (recur)))))
