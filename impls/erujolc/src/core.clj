(ns core
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            printer
            reader
            types))

(defn malify-val [x]
  (if (satisfies? printer/MalPrinter x)
    x
    (let [typ (cond
                (nil? x) :nil
                (instance? clojure.lang.Atom x) :atom
                (seq? x) :list
                (vector? x) :vector
                (boolean? x) :bool
                (string? x) :string
                (keyword? x) :keyword
                (map? x) :map
                (fn? x) :host-fn
                (int? x) :int
                :else :undetermined)]
      (types/->MalDatum typ x))))

(defn malify-fn [f]
  (fn [& args]
    (malify-val (apply f (map :datum-val args)))))

(defn malify-fn-might-throw [f]
  (fn [& args]
    (try
      (malify-val (apply f (map :datum-val args)))
      (catch Exception e
        (throw (ex-info (format "Caught exception: %s" e)
                        {:cause :host-lang-exception
                         :value e}))))))

(defn mal-prn [& xs]
  (println (str/join " " (mapv #(printer/mal-print-string % true) xs)))
  types/mal-nil)


(defn mal-println [& xs]
  (println (str/join " " (mapv #(printer/mal-print-string % false) xs)))
  types/mal-nil)

(defn mal-pr-str [& xs]
  (types/->MalDatum :string (str/join " " (mapv #(printer/mal-print-string % true) xs))))

(defn mal-str [& xs]
  (types/->MalDatum :string (str/join (mapv #(printer/mal-print-string % false) xs))))

(defn mal-read-string [s]
  (when-not (= (:typ s) :string)
    (throw (ex-info (format "cannot read-string a non-string value: %s" (:typ s))
                    {:cause :read-string-non-string
                     :value s})))
  ;; toss the reader
  (second (reader/mal-read-string (:datum-val s))))

(defn mal-slurp [path]
  (when-not (= (:typ path) :string)
    (throw (ex-info (format "cannot slurp a non-string value: %s" (:typ path))
                    {:cause :slurp-non-string
                     :value path})))
  (-> path
      :datum-val
      slurp
      (->>
        (types/->MalDatum :string))))

(defn mal-list [& items]
  (types/mal-datum :list
                   (if items
                     (vec items)
                     [])))

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

(def built-in-env
  "Each must take and return mal data"
  [['+ (malify-fn clojure.core/+)]
   ['- (malify-fn clojure.core/-)]
   ['* (malify-fn clojure.core/*)]
   ['/ (malify-fn clojure.core//)]
   ['prn mal-prn]
   ['println mal-println]
   ['pr-str mal-pr-str]
   ['str mal-str]
   ['read-string mal-read-string]
   ['list mal-list] ;; this is why (list ...) works. I keep forgetting.
   ['list? mal-list?]
   ['nth (malify-fn-might-throw clojure.core/nth)]
   ['first (malify-fn clojure.core/first)]
   ['rest (malify-fn clojure.core/rest)]
   ['empty? mal-empty?]
   ['count mal-count]
   ['= (mal-comp-fn =)]
   ['< (mal-comp-fn <)]
   ['> (mal-comp-fn >)]
   ['<= (mal-comp-fn <=)]
   ['>= (mal-comp-fn >=)]
   ])


