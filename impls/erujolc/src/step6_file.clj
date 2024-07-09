(ns step6-file
  (:require [clojure.string :as str]
            [clojure.pprint]
            core
            env
            printer
            reader
            types
            utils))

;; ========================================
;; REPL Environment
(declare EVAL)

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
  (loop [x x
         env env]
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

          ;; do
          (types/->MalDatum :symbol 'do)
          (do
            (dorun (map #(EVAL % env) (butlast args)))
            (recur (last args) env))

          ;; if
          (types/->MalDatum :symbol 'if)
          (let [[condition then & [else]] args
                condition? (:datum-val (EVAL condition env))]
            (if-not (contains? #{nil false} condition?)
              (recur then env)
              (if else
                (recur else env)
                (types/->MalDatum :nil nil))))

          ;; eval
          (types/->MalDatum :symbol 'eval)
          (let [[ast & _] args]
            (EVAL (EVAL ast env) env))

          ;; fn*
          (types/->MalDatum :symbol 'fn*)
          (let [[binds body] args]
            (types/->MalDatum
              :fn*
              {:ast body
               :binds binds
               :f-env env}))

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
              (recur form env2)))

          ;; read-string
          (types/->MalDatum :symbol 'read-string)
          (core/mal-read-string (EVAL (first args) env))

          ;; slurp
          (types/->MalDatum :symbol 'slurp)
          ;; TODO: handle error from multiple args
          ;; TODO: handle arity for all the special forms!
          (core/mal-slurp (EVAL (first args) env))

          ;; type
          (types/->MalDatum :symbol 'type)
          (let [[x & _error-handling-someday] args]
            (types/->MalDatum :type
                              (name (:typ (EVAL x env)))))

          ;; assume it's a function of some kind
          (let [[f & args] (:datum-val (eval-ast x env))]
            ;; don't print env here, dork. it's got recursive structure in it.
            (condp = (:typ f)
              :host-fn (apply (:datum-val f) args)
              :fn* (let [{:keys [ast binds f-env]} (:datum-val f)
                         e2 (env/mal-environer f-env (:datum-val binds) args)]
                     (recur ast e2))
              (throw (ex-info (format "Unknown type of function: %s" (:typ f))
                              {:cause :unknown-type-of-function
                               :f f
                               :args args})))))))))

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
  #_
  (clojure.pprint/pprint {:moquist :PRINT :form form})
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
      ;; TODO: stop printing here, that's dumb
      (when result (println result))
      (when (and reader (not= :reader/peeked-into-the-abyss (reader/mal-peek reader)))
        (recur (rep reader env))))
    (catch clojure.lang.ExceptionInfo e
      (binding [*out* *err*]
        (prn e)))))

(defn gen-env
  "Given a vector of vectors with symbol-fn pairs, reduce to a MalEnvironer."
  [env-init]
  (let [ks (map #(->> % first (types/->MalDatum :symbol)) env-init)
        vs (map #(->> % second (types/->MalDatum :host-fn)) env-init)]
    (env/mal-environer nil ks vs)))

(defn prompt
  "Print a prompt, read a line.

  Return the input line."
  []
  (print "user> ")
  (flush)
  (some-> (read-line) str/trim))

(defn extend-env
  "Add to the mutable env."
  [env]
  (rep "(def! not (fn* (a) (if a false true)))" env)
  )

(defn -main
  "Prompt for input, process the input with READ-EVAL-PRINT, and recur."
  []
  (let [env (gen-env core/built-in-env)]
    (extend-env env)
    (loop []
      (when-let [input (prompt)]
        (LOOP input env)
        (recur)))))
