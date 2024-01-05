(ns core
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            printer
            types))

(defn malify-fn [f]
  (fn [& args]
    (types/->MalDatum :undetermined
                      (apply f (map :datum-val args)))))

#_
(defn mal-prn [& xs]
  (dorun
    (for [x xs]
      (some-> (printer/mal-print-string x true)
              println)))
  (types/->MalDatum :nil nil))

(defn mal-prn [& xs]
  (println (str/join " " (mapv #(printer/mal-print-string % true) xs)))
  (types/->MalDatum :nil nil))


(defn mal-println [& xs]
  #_
  (dorun
    (for [x xs]
      (some-> (printer/mal-print-string x false)
              println)))
  (println (str/join " " (mapv #(printer/mal-print-string % false) xs)))
  (types/->MalDatum :nil nil))

(defn mal-pr-str [& xs]
  (types/->MalDatum :string (str/join " " (mapv #(printer/mal-print-string % true) xs))))

(defn mal-str [& xs]
  (types/->MalDatum :string (str/join (mapv #(printer/mal-print-string % false) xs)))
  )

(defn mal-list [& items]
  (types/->MalDatum :list (or items [])))

(defn mal-list? [x]
  (->> x :typ (= :list) (types/->MalDatum :bool)))

(defn mal-empty? [x]
  (types/->MalDatum
    :bool
    (or (and (-> x :typ (#{:list :vector :map :set}))
             (-> x :datum-val count zero?))
        (-> x :typ (= :nil)))))

(defn mal-count [x]
  (if (-> x :typ (#{:list :vector :map :set :nil}))
    (types/->MalDatum :int
                      (-> x :datum-val count))))

#_
(defn mal-comp-fn [f]
  (fn [& args]
    (types/->MalDatum :bool
                       (apply f (map :datum-val args)))))

(= (type (types/->MalDatum :int 7)) types.MalDatum)

(defn mal-comp-fn [f]
  (fn [& args]
    (let [args2 (map #(walk/prewalk
                        (fn [x]
                          (if (= (type x) types.MalDatum)
                            (:datum-val x)
                            x))
                        %)
                     args)]
      (types/->MalDatum :bool (apply f args2)))))

(def built-in-env [['+ (malify-fn clojure.core/+)]
                   ['- (malify-fn clojure.core/-)]
                   ['* (malify-fn clojure.core/*)]
                   ['/ (malify-fn clojure.core//)]
                   ['prn mal-prn]
                   ['println mal-println]
                   ['pr-str mal-pr-str]
                   ['str mal-str]
                   ['list mal-list]
                   ['list? mal-list?]
                   ['empty? mal-empty?]
                   ['count mal-count]
                   ['= (mal-comp-fn =)]
                   ['< (mal-comp-fn <)]
                   ['> (mal-comp-fn >)]
                   ['<= (mal-comp-fn <=)]
                   ['>= (mal-comp-fn >=)]
                   ])


