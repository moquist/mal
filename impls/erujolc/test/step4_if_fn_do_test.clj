(ns step4-if-fn-do-test
  (:require [clojure.test :refer [testing deftest is]]
            [step4-if-fn-do :as step4]
            types))

(comment
  (step4/READ "(let* (b (+ 1 2 (* 3 4 (/ 5 6)))) (* b b))")
  )

(def input-string
  "(let* [b (+ 1 2 (* 3 4 (/ 5 6)))] (* b b))")

(def expected-ast
  (types/->MalDatum
    :list,
    [
     (types/->MalDatum :symbol, 'let*)
     (types/->MalDatum
       :vector,
       [
        (types/->MalDatum :symbol, 'b)
        (types/->MalDatum
          :list,
          [
           (types/->MalDatum :symbol, '+)
           (types/->MalDatum :int, 1)
           (types/->MalDatum :int, 2)
           (types/->MalDatum
             :list,
             [
              (types/->MalDatum :symbol, '*)
              (types/->MalDatum :int, 3)
              (types/->MalDatum :int, 4)
              (types/->MalDatum
                :list,
                [
                 (types/->MalDatum :symbol, '/)
                 (types/->MalDatum :int, 5)
                 (types/->MalDatum :int, 6)])])])])
     (types/->MalDatum
       :list,
       [
        (types/->MalDatum :symbol, '*)
        (types/->MalDatum :symbol, 'b)
        (types/->MalDatum :symbol, 'b)])]))

(deftest READ
  (is (= (second (step4/READ input-string)) expected-ast)))

(deftest EVAL
  (is (= (step4/EVAL expected-ast (step4/gen-env core/built-in-env))
         (types/->MalDatum :undetermined 169N)))
  (let [[_ form] (step4/READ "(let* [a 1, b (* a 2)] (* b b))")]
    (is (= (step4/EVAL form (step4/gen-env core/built-in-env))
           (types/->MalDatum :undetermined 4)))))

(deftest eval-ast-wut
  (let [[_ form] (step4/READ "(list 1 2 3)")]
    (is (= form 
           (types/->MalDatum :list
                             [(types/->MalDatum :symbol 'list)
                              (types/->MalDatum :int 1)
                              (types/->MalDatum :int 2)
                              (types/->MalDatum :int 3)
                              ])))
    (is (= (step4/EVAL form (step4/gen-env core/built-in-env))
           (types/->MalDatum :list [(types/->MalDatum :int 1)
                                    (types/->MalDatum :int 2)
                                    (types/->MalDatum :int 3)])))
    ))



