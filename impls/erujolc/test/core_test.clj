(ns core-test
  (:require [clojure.test :refer [deftest is testing]]
            core
            types))

(deftest malify-fn-test
  (is (= ((core/malify-fn +)
          (types/->MalDatum :int 3)
          (types/->MalDatum :int 4))
         (types/->MalDatum :undetermined 7))))

(deftest mal-prn-test
  (let [stdout (with-out-str
                 (let [x (core/mal-prn (types/->MalDatum :int 77)
                                       (types/->MalDatum :keyword :hello)
                                       (types/->MalDatum :string "meeko"))]
                   (is (= x types/mal-nil))))]
    (is (= stdout "77 :hello \"meeko\"\n"))))

(deftest mal-println-test
  (let [stdout (with-out-str
                 (let [x (core/mal-println (types/->MalDatum :int 77)
                                           (types/->MalDatum :keyword :hello)
                                           (types/->MalDatum :string "meeko"))]
                   (is (= x types/mal-nil))))]
    (is (= stdout "77 :hello meeko\n"))))

(deftest mal-pr-str-test
  (is (= (core/mal-pr-str (types/->MalDatum :int 7))
         (types/->MalDatum :string "7"))))

(deftest mal-str-test
  (is (= (core/mal-str
           (types/mal-datum :int 7)
           (types/mal-datum :keyword :hi)
           (types/mal-datum :string "bonanza")
           (types/mal-datum :bool false))
         (types/mal-datum :string "7:hibonanzafalse"))))

(deftest mal-list-test
  (is (= (core/mal-list (types/mal-datum :int 7)
                        (types/mal-datum :list [(types/mal-datum :int 8)
                                                (types/mal-datum :keyword :hello)]))
         (types/mal-datum :list
                          [(types/mal-datum :int 7)
                           (types/mal-datum :list [(types/mal-datum :int 8)
                                                   (types/mal-datum :keyword :hello)]) ])))
  (is (= (core/mal-list) (types/mal-datum :list []))))

(deftest mal-list?-test
  (is (= (core/mal-list? nil)
         (types/mal-datum :bool false)))
  (is (= (core/mal-list? {:typ :list})
         (types/mal-datum :bool true))))

(deftest mal-empty?-test
  (is (= (core/mal-empty? (types/mal-datum :list []))
         (types/mal-datum :bool true)))
  (is (= (core/mal-empty? (types/mal-datum :set []))
         (types/mal-datum :bool true)))
  (is (= (core/mal-empty? (types/mal-datum :nil nil))
         (types/mal-datum :bool true)))
  (is (= (core/mal-empty? (types/mal-datum :list [(types/mal-datum :int 1)]))
         (types/mal-datum :bool false))))

(deftest mal-count-test
  (is (= (core/mal-count (types/mal-datum :list []))
         (types/mal-datum :int 0)))
  (is (= (core/mal-count (core/mal-list (types/mal-datum :int 1)
                                        (types/mal-datum :int 2)))
         (types/mal-datum :int 2))))

(deftest mal-comp-fn-test
  (is (= ((core/mal-comp-fn <)
          (types/mal-datum :int 3)
          (types/mal-datum :int 4))
         (types/mal-datum :bool true))))
