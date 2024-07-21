(ns step6-file
  (:require [clojure.string :as str]
            [clojure.pprint]
            core
            env
            printer
            reader
            types
            utils))

#_ ;; too crude for avoiding recursive env-printing
(set! *print-level* 2)

;; ========================================
;; REPL Environment
(declare EVAL)

(defn mal-def!
  "Set k to the evaluated form in the outermost env, returning the evaluated form value."
  [env k form]
  (let [v (EVAL form env)]
    (env/def env k v)
    v))

(def mal-specials
  {(types/->MalDatum :symbol 'def!) mal-def!})

(def mal-atoms
  ;; could just use clojure atoms, but seems interesting to have a slightly different model
  "map from atom-id to value"
  (atom {}))

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
          ;; atom
          (types/->MalDatum :symbol 'atom)
          (let [[v & _err] args
                atom-id (gensym)]
            (swap! mal-atoms assoc atom-id (EVAL v env))
            (-> (types/mal-datum :atom atom-id)
                (assoc :meta-datum {:mal-atoms mal-atoms})))

          ;; deref
          (types/mal-datum :symbol 'deref)
          (let [[a & _err] args
                a (EVAL a env)
                atom-id (:datum-val a)]
            (-> mal-atoms deref (get atom-id)))

          ;; reset
          (types/mal-datum :symbol 'reset!)
          (let [[a v & _err] args
                a (EVAL a env)
                v (EVAL v env)
                atom-id (:datum-val a)]
            (swap! mal-atoms assoc atom-id v)
            v)

          ;; swap!
          (types/mal-datum :symbol 'swap!)
          (let [[a f & f-args] (:datum-val (eval-ast (types/mal-datum :list args) env))
                atom-id (:datum-val a)
                atom-val (-> mal-atoms deref (get atom-id))
                new-val (EVAL
                          (types/mal-datum
                            :list
                            (into
                              [f atom-val]
                              f-args))
                          env)]
            (swap! mal-atoms assoc atom-id new-val)
            new-val)

          ;; env-keys
          (types/mal-datum :symbol 'env-keys)
          (prn (->> env :data deref keys (map :datum-val)))

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
          (let [[ast & _] args
                outermost-env (env/outermost env)]
            #_ ;; wrong but I do not undertand why right now.
            ;; likely ast isn't an ast... wrong type
            (EVAL (eval-ast ast env) env)
            (EVAL (EVAL ast env)
                  outermost-env))

          ;; fn*
          (types/->MalDatum :symbol 'fn*)
          (let [[binds body] args]
            (with-meta
              (types/->MalDatum
                :fn*
                {:ast body
                 :binds binds
                 :f-env env})
              {:type :types/fn*}))

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
                               ;; #_#_ ;; recursive, blows up!
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
  (rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))" env)
  (rep "(def! atom? (fn* (x) (= \"atom\" (type x))))" env)
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
