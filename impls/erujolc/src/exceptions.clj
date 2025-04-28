(ns exceptions
  (:require types))

(def mal-exception-state-default
  {:exceptions []
   :thrown? false})

(def mal-exception-state
  ;; could just use Clojure exceptions, but will learn more by implementing without
  (atom mal-exception-state-default))

(defn mal-exception-thrown? []
  (:thrown? @mal-exception-state))

(defn mal-exception-get [& [c]]
  (-> mal-exception-state
      deref
      :exceptions
      last
      (or (throw (ex-info "Attempting to get mal exception when there is none thrown"
                          {:erujolc? true})))))

(defn mal-exception-reset! []
  (reset! mal-exception-state mal-exception-state-default))

(defn throw-mal-exception! [msg]
  (when-not (:typ msg)
    (throw (ex-info (format "Attempted to throw non-mal-type as exception: %s" msg)
                    {:cause :exception-thrown-without-mal-type
                     :erujolc? true
                     :msg msg})))
  (swap! mal-exception-state
         (fn [a]
           (-> a
               (assoc :thrown? true)
               (update :exceptions
                       conj
                       (types/mal-datum :exception msg))))))


