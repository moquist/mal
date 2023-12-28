(ns env
  (:refer-clojure :exclude [get find set]))

(defprotocol MalEnviron
  (-set [this k v]
    "Add this kv pair to env.")
  (-find [this k]
    "Recursively search this env and then its parents for k. Return the env containing k.")
  (-get [this k]
    "Using -find, look up k in this env or a parent and return the matching v."))

(defn set [this k v]
  (-set this k v))

(defn find [this k]
  (-find this k))

(defn get [this k]
  (-get this k))

(defrecord MalEnvironer [outer data]
  MalEnviron
  (-set [this k v]
    (assoc-in this [:data k] v))
  (-find [this k]
    (cond
      (contains? data k) this
      (satisfies? MalEnviron outer) (-find outer k)
      :else nil))
  (-get [this k]
    (let [{data :data :as monkey} (-find this k)]
      (if (contains? data k)
        (data k)
        (throw (ex-info (format "%s not found, total bummer" k)
                        {:cause :ns-resolve-failed
                         :env this}))))))

(defn mal-environer [outer & [data-init]]
  (->MalEnvironer outer (or data-init {})))
