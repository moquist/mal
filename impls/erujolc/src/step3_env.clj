(ns step3-env
  (:require [clojure.string :as str]
            env
            printer
            reader
            types
            utils))

;; ========================================
;; REPL Environment
(declare EVAL)

(def built-in-env [['+ clojure.core/+]
                   ['- clojure.core/-]
                   ['* clojure.core/*]
                   ['/ clojure.core//]])

(defn mal-def!
  "Set k to the evaluated form in env, returning env"
  [env k form]
  (let [v (EVAL form env)]
    (env/set env k v)
    v))

(def mal-specials
  {(types/->MalDatum :symbol 'def!) mal-def!})

;; ========================================
;; EVAL
(declare eval-ast)
(defn EVAL
  "x and return value are always MalDatum"
  [x env]
  (cond
    (-> x :typ (not= :list))
    (eval-ast x env)

    (-> x :datum-val empty?)
    (types/->MalDatum :list [])

    :else
    (let [[f & args] (:datum-val x)]
      (condp = f
        ;; def!
        (types/->MalDatum :symbol 'def!)
        (apply mal-def! env args)

        ;; let*
        (types/->MalDatum :symbol 'let*)
        ;; TODO: allow more than one form
        (let [[bindings form] args]
          (when-not (-> bindings :typ (#{:vector :list}))
            (throw (ex-info "bindings form must be a list or a vector"
                            {:cause :bindgings-form-non-vector
                             :bindings bindings})))
          (when (-> bindings :datum-val count odd?)
            (throw (ex-info "bindings vector must have an even number of forms"
                            {:cause :bindings-vector-odd-number-of-forms
                             :bindings bindings})))
          (let [[env2 & _] (reduce (fn [[r & _] [k v]]
                                     (env/set r k (EVAL v r)))
                                   [(env/mal-environer env nil nil)]
                                   (->> bindings :datum-val (partition 2)))]
            (EVAL form env2)))

        (let [[f & args] (:datum-val (eval-ast x env))]
          (types/->MalDatum :undetermined
                            (apply (:datum-val f) (map :datum-val args))))))))

(defn eval-ast
  "ast and return value are always MalDatum"
  [ast env]
  (condp = (:typ ast)
    :symbol (or
              (env/get env ast)
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
  (when (satisfies? printer/MalPrinter form)
    (printer/mal-print-string form true)))

(defn rep [x env]
  (let [[reader form] (READ x)]
    [reader (-> form (EVAL env) PRINT)]))

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

(defn gen-env
  "Given a vector of vectors with symbol-fn pairs, reduce to a MalEnvironer."
  [env-init]
  (reduce (fn [[r & _] [sym f]]
            (env/set r
                     (types/->MalDatum :symbol sym)
                     (types/->MalDatum :fn f)))
          [(env/mal-environer nil nil nil)]
          env-init))

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
  (let [[env & _] (gen-env built-in-env)]
    (loop []
      (when-let [input (prompt)]
        (LOOP input env)
        (recur)))))
