(ns step3-env-test
  (:require [clojure.test :refer [testing deftest is]]
            [step3-env :as step3]
            types))

(comment
  (step3/READ "(let* (b (+ 1 2 (* 3 4 (/ 5 6)))) (* b b))")
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
  (is (= (second (step3/READ input-string)) expected-ast)))

(deftest EVAL
  (is (= (step3/EVAL expected-ast (first (step3/gen-env step3/built-in-env)))
         (types/->MalDatum :undetermined 169N)))
  (let [[_ form] (step3/READ "(let* [a 1, b (* a 2)] (* b b))")]
    (is (= (step3/EVAL form (first (step3/gen-env step3/built-in-env)))
           (types/->MalDatum :undetermined 4)))))

(deftest eval-ast-wut
  (let [[_ form] (step3/READ "(list 1 2 3)")]
    (is (= form 
           (types/->MalDatum :list
                             [(types/->MalDatum :symbol 'list)
                              (types/->MalDatum :int 1)
                              (types/->MalDatum :int 2)
                              (types/->MalDatum :int 3)
                              ])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (step3/EVAL form (first (step3/gen-env step3/built-in-env)))))
    ))
