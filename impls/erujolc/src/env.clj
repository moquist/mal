(ns env
  (:require types)
  (:refer-clojure :exclude [get find set]))

(defprotocol MalEnviron
  (-set [this k v]
    "Add this kv pair to env.")
  (-def [this k v]
    "Add this kv pair to the outermost env. This is to support 'def from inside a 'fn with the fn's local bind-context env.")
  (-outermost [this] "Return the outermost env.")
  (-find [this k]
    "Recursively search this env and then its parents for k. Return the env containing k.")
  (-get [this k]
    "Using -find, look up k in this env or a parent and return the matching v. Throws clojure.lang.ExceptionInfo if k is not found."))

(defn set [this k v]
  (-set this k v)
  [this v])

(defn def [this k v]
  (-def this k v)
  [this v])

(defn outermost [this]
  (-outermost this))

(defn find [this k]
  (-find this k))

(defn get [this k]
  (-get this k))

(defn get-safe
  "Like get, but returns ::not-found on failed lookup."
  [this k]
  (try
    (get this k)
    (catch clojure.lang.ExceptionInfo _
      ::not-found)))

(defrecord MalEnvironer [outer data]
  MalEnviron
  (-set [this k v]
    (swap! data assoc k v)
    this
    #_ ;; maybe immutable at some point.... but it seems hard.
    (assoc-in this [:data k] v))
  (-def [this k v]
    (-set (-outermost this) k v))
  (-outermost [this]
    (loop [{:keys [outer] :as x} this]
      (if (satisfies? env/MalEnviron outer)
        (recur outer)
        x)))
  (-find [this k]
    (cond
      (contains? @data k) this
      (satisfies? MalEnviron outer) (-find outer k)
      :else nil))
  (-get [this k]
    (let [{data :data} (-find this k)]
      (if (and data (contains? @data k))
        (@data k)
        (throw (ex-info (format "%s not found, total bummer" (:datum-val k))
                        {:cause :ns-resolve-failed
                         :key k
                         ;; dont' print 'this, it's recursive... blows up!
                         :env this})))))
  #_#_
  Object
  (toString [this]
    (format "#<MalEnvironer>%s" @(:data this))))

(defmethod clojure.core/print-method MalEnvironer [env, writer]
  (prn (format "#<MalEnvironer>%s" (-> env :data deref keys))))

(defn handle-variadic [binds exprs]
  (loop [binds binds
         exprs exprs
         result {}]
    (if (-> binds
            first
            (= (types/->MalDatum :symbol '&)))
      (assoc result
             (second binds)
             (types/->MalDatum :list exprs))
      (let [result (assoc result
                          (first binds)
                          (first exprs))]
        (if (next binds)
          (recur (next binds)
                 (next exprs)
                 result)
          result)))))

(defn mal-environer [outer binds exprs]
  (let [init-data (handle-variadic binds exprs)]
    (->MalEnvironer outer (atom init-data))))
