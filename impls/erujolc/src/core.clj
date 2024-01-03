(ns core
  (:require printer
            types))

(defn malify-fn [f]
  (fn [& args]
    (types/->MalDatum :undetermined
                      (apply f (map :datum-val args)))))

(defn mal-prn [& xs]
  (dorun
    (for [x xs]
      (some-> (printer/mal-print-string x true)
              println)))
  (types/->MalDatum :nil nil))

(defn mal-list [& items]
  (types/->MalDatum :list items))

(defn mal-list? [x]
  (->> x :typ (= :list) (types/->MalDatum :bool)))

(defn mal-empty? [x]
  (or (and (-> x :typ (#{:list :vector :map :set}))
           (-> x :datum-val count zero?))
      (->> x :typ (= :nil) (types/->MalDatum :bool))))

(defn mal-count [x]
  (if (-> x :typ (#{:list :vector :map :set}))
    (types/->MalDatum :int
                      (-> x :datum-val count))))

(defn mal-comp-fn [f]
  (fn [& args]
    (types/->MalDatum :bool
                       (apply f (map :datum-val args)))))

(def built-in-env [['+ (malify-fn clojure.core/+)]
                   ['- (malify-fn clojure.core/-)]
                   ['* (malify-fn clojure.core/*)]
                   ['/ (malify-fn clojure.core//)]
                   ['prn mal-prn]
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


