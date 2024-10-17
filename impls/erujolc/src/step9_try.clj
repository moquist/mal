(ns step9-try
  (:require [clojure.string :as str]
            [clojure.pprint]
            core
            env
            exceptions
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

(defn mal-defmacro!
  "Set k to the evaluated form in the outermost env, returning the evaluated form value. Differs from def! by assuming that the :datum-val of v is a map, and setting macro? true in that map."
  [env k form]
  (let [v (EVAL form env)]
    (env/def
      env
      k
      (assoc-in v [:datum-val :macro?] true))
    v))

(def mal-atoms
  ;; could just use clojure atoms, but seems interesting to have a slightly different model
  "map from atom-id to value"
  (atom {}))

(declare quasiquote)
(defn quasiquote-helper [{:keys [typ datum-val] :as ast}]
  (let [x (if (empty? datum-val)
            (types/mal-datum :list []) ; mal tests require that empty ast always be a list, not a vec
            (let [[elt & elts] datum-val
                  {elt-typ :typ elt-datum-val :datum-val} elt
                  mal-elts (types/mal-datum :list elts)]
              (if (and (= elt-typ :list)
                       (-> elt-datum-val first (= (types/mal-datum :symbol 'splice-unquote))))
                (types/mal-datum :list [(types/mal-datum :symbol 'concat)
                                        (second elt-datum-val)
                                        (quasiquote-helper mal-elts)])
                (types/mal-datum :list [(types/mal-datum :symbol 'cons)
                                        (quasiquote elt)
                                        (quasiquote-helper mal-elts)]))))]
    (if (= :list typ)
      x
      (types/mal-datum :list [(types/mal-datum :symbol 'vec)
                              x]))))

(defn quasiquote [ast]
  (let [{:keys [typ datum-val]} ast]
    (cond
      (and (= :list typ)
           (-> datum-val first (= (types/mal-datum :symbol 'unquote))))
      (second datum-val)

      (#{:list :vector} typ)
      (quasiquote-helper ast)

      (#{:map :symbol} typ)
      (types/mal-datum :list
                       [(types/mal-datum :symbol 'quote)
                        ast])

      :else ast)))

(defn ast->maybe-macro-call
  "If ast contains a macro call, return the macro. Else return falsy."
  [{:as ast :keys [typ datum-val]} env]
  (when (= typ :list)
    (let [[{first-elem-typ :typ :as first-elem} & args] datum-val]
      (when (= first-elem-typ :symbol)
        (let [{:keys [typ datum-val]} (env/get-safe env first-elem)]
          (and (= typ :fn*)
               (-> datum-val :macro?)
               {:macro-f datum-val
                :macro-args args}))))))

(defn mal-macroexpand [ast env]
  ;; N.B.: env is mutable, no need to bind it in the loop.
  (loop [ast ast]
    (if-let [macro-deets (ast->maybe-macro-call ast env)]
      (let [{:keys [macro-f macro-args]} macro-deets
              {:keys [ast binds f-env]} macro-f
              e2 (env/mal-environer f-env (:datum-val binds) macro-args)]
          (EVAL ast e2 true))
      ast)))

;; ========================================
;; EVAL
(declare eval-ast)
(defn EVAL
  "x and return value are always MalDatum"
  ([x env]
   (EVAL x env true))
  ([x env macroexpand-moquist?]
   #_
   (prn :moquist-x x)
   (if (exceptions/mal-exception-thrown?)
     ::mal-exception-thrown
     (let [x (if-not macroexpand-moquist?
               x
               (mal-macroexpand x env))]
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

             ;; throw
             (types/mal-datum :symbol 'throw)
             ;; TODO: eval-ast here? Then might have exception-thrown-while-throwing-exception
             (let [[e & _err] args]
               (exceptions/throw-mal-exception! e))

             ;; try*/catch*
             (types/mal-datum :symbol 'try*)
             (let [[form & [catch-block]] args
                   tried (EVAL form env)]
               (if-not (exceptions/mal-exception-thrown?)
                 tried
                 (let [[_catch* exception-symbol exception-form] (:datum-val catch-block)
                       e2 (env/mal-environer env [exception-symbol] [(exceptions/mal-exception-get)])]
                   (exceptions/mal-exception-reset!)
                   (recur exception-form e2 true))))

             ;; exception
             (types/mal-datum :symbol 'exception)
             (let [evaluated-ast (eval-ast args env)]
               (when-not (exceptions/mal-exception-thrown?)
                 (let [[e & _err] evaluated-ast]
                   (types/mal-datum :exception e))))

             ;; cons
             (types/mal-datum :symbol 'cons)
             (let [[x lst] (map #(EVAL % env) args)]
               ;; TODO: ensure lst is a :list
               (types/mal-datum :list
                                (into [x] (:datum-val lst))))

             ;; concat
             (types/mal-datum :symbol 'concat)
             (let [lists (map #(:datum-val (EVAL % env)) args)]
               ;; TODO: ensure lst is a :list
               (types/mal-datum :list
                                (vec (apply concat lists))))

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
             (let [ast-evaluated (eval-ast (types/mal-datum :list args) env)]
               (when-not (exceptions/mal-exception-thrown?)
                 (let [[a f & f-args] (:datum-val ast-evaluated)
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
                   new-val)))

             ;; env-keys
             (types/mal-datum :symbol 'env-keys)
             (prn (->> env :data deref keys (map :datum-val)))

             ;; def!
             (types/->MalDatum :symbol 'def!)
             (apply mal-def! env args)

             ;; defmacro!
             (types/->MalDatum :symbol 'defmacro!)
             (apply mal-defmacro! env args)

             ;; do
             (types/->MalDatum :symbol 'do)
             (do
               (dorun (map #(EVAL % env) (butlast args)))
               (recur (last args) env true))

             ;; if
             (types/->MalDatum :symbol 'if)
             (let [[condition then & [else]] args
                   condition? (:datum-val (EVAL condition env))]
               (if-not (contains? #{nil false} condition?)
                 (recur then env true)
                 (if else
                   (recur else env true)
                   (types/->MalDatum :nil nil))))

             ;; eval
             (types/->MalDatum :symbol 'eval)
             (let [[ast & _] args
                   outermost-env (env/outermost env)]
               (EVAL (EVAL ast env)
                     outermost-env))

             ;; macroexpand
             (types/mal-datum :symbol 'macroexpand)
             (let [[ast & _] args]
               (mal-macroexpand ast env))

             ;; fn*
             (types/->MalDatum :symbol 'fn*)
             (let [[binds body] args]
               (with-meta
                 (types/->MalDatum
                   :fn*
                   {:ast body
                    :binds binds
                    :f-env env
                    :macro? false})
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
                 (recur form env2 true)))

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

             (types/mal-datum :symbol 'quote)
             (first args)

             (types/mal-datum :symbol 'quasiquoteexpand)
             (quasiquote (first args))

             (types/mal-datum :symbol 'vec)
             (let [[arg & _too-many-args?] args
                   arg (EVAL arg env)
                   {:keys [typ datum-val]} arg]
               (if-not (#{:list :vector} typ)
                 (throw (ex-info "argument to vec must be a list or a vector"
                                 {:cause :vec-argument-must-be-list-or-vector
                                  :arg arg}))
                 (types/mal-datum :vector datum-val)))

             (types/mal-datum :symbol 'quasiquote)
             (recur (quasiquote (first args)) env true)

             ;; map
             (types/mal-datum :symbol 'map)
             (let [[f coll] args ; do not eval-ast f
                   evaluated-coll (EVAL coll env)]
               (when-not (exceptions/mal-exception-thrown?)
                 (let [results (mapv #(EVAL
                                       (types/mal-datum :list [f %])
                                       env)
                                     (:datum-val evaluated-coll))]
                   (types/mal-datum :list results))))

             ;; apply
             (types/mal-datum :symbol 'apply)
             (let [applied-coll (last args)
                   applied-coll-evaluated (EVAL applied-coll env)]
               (prn :moquist-applied-coll-evaluated applied-coll-evaluated)
               (when-not (exceptions/mal-exception-thrown?)
                 (let [f-and-args (into (vec (butlast args)) applied-coll-evaluated)]
                   (recur (types/mal-datum :list f-and-args)
                          env
                          false))))
             #_
             (let [applied-coll (last args)
                   applied-coll-evaluated (eval-ast applied-coll env)]
               (prn :moquist-applied-coll-evaluated applied-coll-evaluated)
               (when-not (exceptions/mal-exception-thrown?)
                 (let [f-and-args (into (vec (butlast args)) applied-coll-evaluated)]
                   (recur (types/mal-datum :list f-and-args)
                          env
                          false))))
             #_
             (let [ast-evaluated (EVAL (types/mal-datum :list args) env)]
               #_
               (prn :moquist-apply-ast-evaluated ast-evaluated)
               (when-not (exceptions/mal-exception-thrown?)
                 (let [ast (:datum-val ast-evaluated)
                       #_#_
                       _ (prn :moquist-apply-ast ast)
                       f-and-args (into (vec (butlast ast)) (:datum-val (last ast)))]
                   (recur (types/mal-datum :list f-and-args)
                          env
                          false))))
             #_
             (let [f-and-args (into (vec (butlast args)) (:datum-val (last args)))]
               (prn :moquist-args args)
               (recur (types/mal-datum :list f-and-args)
                      env
                      false))
             #_
             (let [ast-evaluated (eval-ast (types/mal-datum :list args) env)]
               #_
               (prn :moquist-apply-ast-evaluated ast-evaluated)
               (prn :moquist-1)
               (when-not (exceptions/mal-exception-thrown?)
                 (let [ast (:datum-val ast-evaluated)
                       #_#_
                       _ (prn :moquist-apply-ast ast)
                       f-and-args (into (vec (butlast ast)) (:datum-val (last ast)))]
                   (prn :moquist-2 (:datum-val (last ast)))
                   (recur (types/mal-datum :list f-and-args)
                          env
                          false))))

             ;; assume it's a function of some kind
             (let [ast-evaluated (eval-ast x env)]
               (when-not (exceptions/mal-exception-thrown?)
                 (let [[f & args] (:datum-val ast-evaluated)]
                   ;; don't print env here, dork. it's got recursive structure in it.
                   (condp = (:typ f)
                     :host-fn (apply (:datum-val f) args)
                     :fn* (let [{:keys [ast binds f-env]} (:datum-val f)
                                e2 (env/mal-environer f-env (:datum-val binds) args)]
                            (recur ast e2 true))
                     (throw (ex-info (format "Unknown type of function: %s" (:typ f))
                                     {:cause :unknown-type-of-function
                                      :f f
                                      :args args})))))))))))))

(defn eval-ast
  "ast and return value are always MalDatum"
  [ast env]
  (condp = (:typ ast)
    :symbol (or
              (env/get env ast)
              (exceptions/throw-mal-exception! (types/mal-datum :string (format "'%s' not found" (:datum-val ast)))))
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
  (cond
    (exceptions/mal-exception-thrown?)
    (let [x (exceptions/mal-exception-get)]
      (exceptions/mal-exception-reset!)
      (print "Exception: ")
      (printer/mal-print-string x true))

    (satisfies? printer/MalPrinter form)
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
  (rep "(def! load-file (fn* (filepath) (eval (read-string (str \"(do \" (slurp filepath) \"\nnil)\")))))" env)
  (rep "(def! atom? (fn* (x) (= \"atom\" (type x))))" env)
  ;; step 8
  (rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
       env)
  (rep "(def! inc (fn* (a) (+ a 1)))" env)
  (rep "(def! dec (fn* (a) (- a 1)))" env)
  (rep "(def! nil? (fn* (a) (= a nil)))" env)
  (rep "(def! symbol? (fn* (a) (= (type a) \"symbol\")))" env)
  (rep "(def! true? (fn* (a) (= a true)))" env)
  (rep "(def! false? (fn* (a) (= a false)))" env)
  )

(defn -main
  "Prompt for input, process the input with READ-EVAL-PRINT, and recur."
  [& args]
  (let [env (gen-env core/built-in-env)]
    (extend-env env)
    (env/def
      env
      (types/mal-datum :symbol '*ARGV*)
      (types/mal-datum :list (mapv (partial types/mal-datum :string)
                                   (rest args))))
    (if (seq args)
      ;; cli execution
      (rep (format "(load-file \"%s\")" (first args)) env)

      ;; repl
      (loop []
        (when-let [input (prompt)]
          (LOOP input env)
          (recur))))))