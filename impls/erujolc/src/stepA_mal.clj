(ns stepA-mal
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
             #_#_
             (types/->MalDatum :symbol 'atom)
             (let [[v & _err] args
                   atom-id (gensym)]
               (swap! mal-atoms assoc atom-id (EVAL v env))
               (-> (types/mal-datum :atom atom-id)
                   (assoc :meta-datum {:mal-atoms mal-atoms})))

             ;; throw-mal-exception!
             ;; ALT: expose the exception state to the Mal programmer.
             (types/mal-datum :symbol 'throw-mal-exception!)
             (let [[e & _err] args]
               (exceptions/throw-mal-exception! (eval-ast e env)))

             ;; try*/catch*
             (types/mal-datum :symbol 'try*)
             (let [[form & [catch-block]] args
                   tried (EVAL form env)]
               (if-not (exceptions/mal-exception-thrown?)
                 tried
                 (let [mal-e (exceptions/mal-exception-get)]
                   (if-let [catch-block (:datum-val catch-block)]
                     (let [[_catch* exception-symbol exception-form] catch-block
                           e2 (env/mal-environer env [exception-symbol] [(exceptions/mal-exception-get)])]
                       (exceptions/mal-exception-reset!)
                       (recur exception-form e2 true))
                     (do
                       (exceptions/mal-exception-reset!)
                       (throw (ex-info (format "uncaught exception: %s" (printer/mal-print-string mal-e false))
                                       {:cause :uncaught-exception
                                        :erujolc? true
                                        :exception mal-e})))))))

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
             #_#_
             (types/mal-datum :symbol 'concat)
             (let [lists (map #(:datum-val (EVAL % env)) args)]
               ;; TODO: ensure lst is a :list
               (types/mal-datum :list
                                (vec (apply concat lists))))

             ;; deref
             #_#_
             (types/mal-datum :symbol 'deref)
             (let [[a & _err] args
                   a (EVAL a env)
                   atom-id (:datum-val a)]
               (-> mal-atoms deref (get atom-id)))

             ;; reset
             #_#_
             (types/mal-datum :symbol 'reset!)
             (let [[a v & _err] args
                   a (EVAL a env)
                   v (EVAL v env)
                   atom-id (:datum-val a)]
               (swap! mal-atoms assoc atom-id v)
               v)

             ;; env-keys
             (types/mal-datum :symbol 'env-keys)
             (prn (->> env :data deref keys (map :datum-val)))

             ;; (def! a (throw "b"))
             ;; (erujolc-env-spy a)
             ;;   ^--- the exceptions atom is leaked!
             (types/mal-datum :symbol 'erujolc-env-spy)
             (prn (-> env :data deref (get (first args))))

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
                                  :erujolc? true
                                  :bindings bindings})))
               (when (-> bindings :datum-val count odd?)
                 (throw (ex-info "bindings vector must have an even number of forms"
                                 {:cause :bindings-vector-odd-number-of-forms
                                  :erujolc? true
                                  :bindings bindings})))
               (let [[env2 & _] (reduce (fn [[r & _] [k v]]
                                          (env/set r k (EVAL v r)))
                                        [(env/mal-environer env nil nil)]
                                        (->> bindings :datum-val (partition 2)))]
                 (recur form env2 true)))

             ;; read-string
             #_#_
             (types/->MalDatum :symbol 'read-string)
             (core/mal-read-string (EVAL (first args) env))

             ;; slurp
             #_#_
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

             #_#_
             (types/mal-datum :symbol 'vec)
             (let [[arg & _too-many-args?] args
                   arg (EVAL arg env)
                   {:keys [typ datum-val]} arg]
               (if-not (#{:list :vector} typ)
                 (throw (ex-info "argument to vec must be a list or a vector"
                                 {:cause :vec-argument-must-be-list-or-vector
                                  :erujolc? true
                                  :arg arg}))
                 (types/mal-datum :vector datum-val)))

             (types/mal-datum :symbol 'quasiquote)
             (recur (quasiquote (first args)) env true)

             ;; eval-ast debugging
             (types/mal-datum :symbol 'erujolc-eval-ast)
             (do (prn (eval-ast (types/mal-datum :list args) env))
                 (types/mal-datum :nil nil))

             (types/mal-datum :symbol 'erujolc-inspect)
             (do (prn args)
                 (types/mal-datum :nil nil))

             ;; assume it's a function of some kind
             (let [ast-evaluated (eval-ast x env)]
               (when-not (exceptions/mal-exception-thrown?)
                 (let [[f & args] (:datum-val ast-evaluated)]
                   ;; don't print env here, dork. it's got recursive structure in it.
                   (condp = (:typ f)
                     :host-fn (apply (:datum-val f) args)
                     :core-fn (apply (:datum-val f) env args)
                     :fn* (let [{:keys [ast binds f-env]} (:datum-val f)
                                e2 (env/mal-environer f-env (:datum-val binds) args)]
                            (recur ast e2 true))
                     (throw (ex-info (format "Unknown type of function: %s" (:typ f))
                                     {:cause :unknown-type-of-function
                                      :erujolc? true
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
  (try
    (cond
      (string? x) (reader/mal-read-string x)
      (satisfies? reader/MalRead x) (reader/read-form x)
      :else (throw (Exception. (format "READ with invalid input of type %s" (type x)))))
    (catch clojure.lang.ExceptionInfo e
      (if (-> e ex-data :erujolc?)
        (binding [*out* *err*]
          (prn e)
          [nil nil])
        (throw e)))
    (catch Throwable t
      (prn :moquist-READ-throw t)
      [nil nil])))

(defn PRINT [form]
  (if (keyword? form)
    (prn :moquist-PRINT form))
  (cond
    (exceptions/mal-exception-thrown?)
    (let [x (exceptions/mal-exception-get 1)]
      (exceptions/mal-exception-reset!)
      #_
      (prn :moquist-exception x)
      (binding [*out* *err*]
        (println (str "erujolc Exception: " (printer/mal-print-string x true)))))

    (satisfies? printer/MalPrinter form)
    (printer/mal-print-string form true)))

(defn rep [x env]
  (let [[reader form] (READ x)
        evaluation-result (EVAL form env)
        printed-result-str (when-not (= :reader/peeked-into-the-abyss evaluation-result)
                             (PRINT evaluation-result))]
    [reader printed-result-str]))

(defn LOOP
  "Loop through the forms in the provided input"
  [input env]
  (try
    (loop [[reader result] (rep input env)]
      ;; TODO: stop printing here, that's dumb
      (when result (println result) (flush))
      (when (and reader (not= :reader/peeked-into-the-abyss (reader/mal-peek reader)))
        (recur (rep reader env))))
    (catch Throwable e
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; core env
(defn mal-map [env f coll]
  (let [results (mapv
                  (fn [item]
                    (condp = (:typ f)
                      :host-fn ((:datum-val f) [item])
                      :core-fn ((:datum-val f) env [item])
                      :fn* (let [{:keys [ast binds f-env]} (:datum-val f)
                                 e2 (env/mal-environer f-env (:datum-val binds) [item])]
                             (EVAL ast e2 true))
                      (throw (ex-info (format "Unknown type of function mapped: %s" (:typ f))
                                      {:cause :unknown-type-of-function-mapped
                                       :erujolc? true
                                       :f f
                                       :item item}))))
                  (:datum-val coll))]
        (types/mal-datum :list results)))

(defn mal-apply [env f & args]
  (let [args (into (vec (butlast args)) (:datum-val (last args)))]
    ;; don't print env here, dork. it's got recursive structure in it.
    (condp = (:typ f)
      :host-fn (apply (:datum-val f) args)
      :core-fn (apply (:datum-val f) env args)
      :fn* (let [{:keys [ast binds f-env]} (:datum-val f)
                 e2 (env/mal-environer f-env (:datum-val binds) args)]
             (EVAL ast e2 true))
      (do
        #_
        (prn :moquist-f f)
        (throw (ex-info (format "Unknown type of function applied: %s" (:typ f))
                        {:cause :unknown-type-of-function
                         :erujolc? true
                         :f f
                         :args args}))))))

(defn mal-atom [env v]
  (let [atom-id (gensym)]
    (swap! mal-atoms assoc atom-id (EVAL v env))
    (-> (types/mal-datum :atom atom-id)
        (assoc :meta-datum {:mal-atoms mal-atoms}))))

(defn mal-deref [env a]
  (let [a (EVAL a env)
        atom-id (:datum-val a)]
    (-> mal-atoms deref (get atom-id))))

(defn mal-reset [env a v]
  (let [a (EVAL a env)
        v (EVAL v env)
        atom-id (:datum-val a)]
    (swap! mal-atoms assoc atom-id v)
    v))

(defn mal-cons [env & args]
  (let [[x lst] (map #(EVAL % env) args)]
    ;; TODO: ensure lst is a :list
    (types/mal-datum :list
                     (into [x] (:datum-val lst)))))

(defn mal-concat [env & args]
  (let [lists (map #(:datum-val (EVAL % env)) args)]
               ;; TODO: ensure lst is a :list
               (types/mal-datum :list
                                (vec (apply concat lists)))))

(defn mal-read-string [env s]
  (try
    (core/mal-read-string (EVAL s env))
    (catch Throwable _t
      (types/mal-datum :nil nil))))

(defn mal-slurp [env path]
  (core/mal-slurp (EVAL path env)))

(defn mal-vec [env data]
  (let [data (eval-ast data env)
        {:keys [typ datum-val]} data]
    (if-not (#{:list :vector} typ)
      (throw (ex-info "argument to vec must be a list or a vector"
                      {:cause :vec-argument-must-be-list-or-vector
                       :erujolc? true
                       :data data}))
      (types/mal-datum :vector datum-val))))

(defn mal-swap! [env & args]
  (let [[a f & f-args] args
        atom-id (:datum-val a)
        atom-val (-> mal-atoms deref (get atom-id))
        new-val (condp = (:typ f)
                  :host-fn (apply (:datum-val f) atom-val f-args)
                  :core-fn (apply (:datum-val f) env atom-val f-args)
                  :fn* (let [{:keys [ast binds f-env]} (:datum-val f)
                             e2 (env/mal-environer f-env (:datum-val binds) (into [atom-val] f-args))]
                         (EVAL ast e2 true))
                  (throw (ex-info (format "Unknown type of function used in swap!: %s" (:typ f))
                                  {:cause :unknown-type-of-function
                                   :erujolc? true
                                   :f f
                                   :args args})))]
    (swap! mal-atoms assoc atom-id new-val)
    new-val))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
  (rep "(def! true? (fn* (a) (= a true)))" env)
  (rep "(def! false? (fn* (a) (= a false)))" env)
  (rep "(def! throw (fn* (e) (throw-mal-exception! e)))" env)
  (rep "(def! keyword? (fn* (a) (= (type a) \"keyword\")))" env)
  (rep "(def! vector? (fn* (a) (= (type a) \"vector\")))" env)
  (rep "(def! map? (fn* (a) (= (type a) \"map\")))" env)
  (rep "(def! sequential? (fn* (a) (if (= (type a) \"list\") true (if (= (type a) \"vector\") true false))))" env)
  (rep "(def! *host-language* \"erujolc\")" env)
  (rep "(def! time-ms (fn* () (throw \"time-ms is undefined\")))" env)
  (rep "(def! meta (fn* () (throw \"meta is undefined\")))" env)
  (rep "(def! with-meta (fn* () (throw \"with-meta is undefined\")))" env)
  (rep "(def! fn? (fn* () (throw \"fn? is undefined\")))" env)
  (rep "(def! string? (fn* () (throw \"string? is undefined\")))" env)
  (rep "(def! number? (fn* () (throw \"number? is undefined\")))" env)
  (rep "(def! seq (fn* () (throw \"seq is undefined\")))" env)
  (rep "(def! conj (fn* () (throw \"conj is undefined\")))" env)

  ;; :core-fn gets env as the first argument
  ;; :host-fn doesn't get env at all
  (env/set env
           (types/mal-datum :symbol 'map)
           (types/mal-datum :core-fn mal-map))
  (env/set env
           (types/mal-datum :symbol 'apply)
           (types/mal-datum :core-fn mal-apply))
  (env/set env
           (types/mal-datum :symbol 'swap!)
           (types/mal-datum :core-fn mal-swap!))
  (env/set env
           (types/mal-datum :symbol 'atom)
           (types/mal-datum :core-fn mal-atom))
  (env/set env
           (types/mal-datum :symbol 'deref)
           (types/mal-datum :core-fn mal-deref))
  (env/set env
           (types/mal-datum :symbol 'reset!)
           (types/mal-datum :core-fn mal-reset))
  (env/set env
           (types/mal-datum :symbol 'cons)
           (types/mal-datum :core-fn mal-cons))
  (env/set env
           (types/mal-datum :symbol 'concat)
           (types/mal-datum :core-fn mal-concat))
  (env/set env
           (types/mal-datum :symbol 'read-string)
           (types/mal-datum :core-fn mal-read-string))
  (env/set env
           (types/mal-datum :symbol 'slurp)
           (types/mal-datum :core-fn mal-slurp))
  (env/set env
           (types/mal-datum :symbol 'vec)
           (types/mal-datum :core-fn mal-vec)))

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
      (do
        (rep "(println (str \"Mal [\" *host-language* \"]\"))" env)
        (loop []
          (when-let [input (prompt)]
            (LOOP input env)
            (recur)))))))
