(ns step4-if-fn-do
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
        (reduce (fn [r form] (EVAL form env)) nil args)
        #_ ;; Alt according to mal README... not as nice as reduce, IMO.
        (-> (eval-ast (types/->MalDatum :vector args) env)
            :datum-val
            last)

        ;; if
        (types/->MalDatum :symbol 'if)
        (let [[condition then & [else]] args
              condition? (:datum-val (EVAL condition env))]
          (if-not (contains? #{nil false} condition?)
            (EVAL then env)
            (if else
              (EVAL else env)
              (types/->MalDatum :nil nil))))

        ;; fn*
        (types/->MalDatum :symbol 'fn*)
        (let [[binds body] args]
          #_
          (clojure.pprint/pprint {:moquist :fn* :binds binds :body body})
          (types/->MalDatum
            :fn*
            ;; this gets the args already :datum-val from apply, below
            (fn moquist-blookie [& args]
              #_
              (clojure.pprint/pprint {:moquist :blookie :args args :env env})
              (let [e2 (env/mal-environer env (:datum-val binds) args)]
                #_
                (clojure.pprint/pprint {:moquist :blookie2 :e2 e2})
                (EVAL body e2)))))

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

        ;; assume it's a function of some kind
        (let [[f & args] (:datum-val (eval-ast x env))]
          #_
          (clojure.pprint/pprint {:moquist-f f :args args :env env})
          (apply (:datum-val f) args)
          #_
          (condp = (:typ f)
            :host-fn (types/->MalDatum :undetermined
                                       (apply (:datum-val f) (map :datum-val args)))
            :fn* (apply (:datum-val f) args)))))))

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

(defn -main
  "Prompt for input, process the input with READ-EVAL-PRINT, and recur."
  []
  (let [env (gen-env core/built-in-env)]
    (loop []
      (when-let [input (prompt)]
        (LOOP input env)
        (recur)))))
