(ns step1-read-print-test
  (:require [clojure.test :refer [deftest testing is]]
            [step1-read-print :as step1]
            types))

(deftest READ
  (is (= (second (step1/READ "")) :reader/peeked-into-the-abyss))
  (is (= (second (step1/READ "1")) #types.MalDatum{:typ :int :datum-val 1}))
  (is (= (second (step1/READ " 1 ")) #types.MalDatum{:typ :int :datum-val 1}))
  (is (= (second (step1/READ " 1  2")) #types.MalDatum{:typ :int :datum-val 1}))
  (is (= (second (step1/READ "()")) #types.MalDatum{:typ :list :datum-val []}))
  (is (nil? (second (step1/READ "("))))
  )

(deftest PRINT
  (is (= (step1/PRINT (types/->MalDatum :symbol 'a)) "a"))
  (is (= (step1/PRINT (types/->MalDatum
                        :list [(types/->MalDatum :keyword :a)])) "(:a)"))
  (is (= (step1/PRINT (types/->MalDatum
                        :vector [(types/->MalDatum :keyword :a)])) "[:a]"))
  (is (= "false" (step1/PRINT (types/mal-datum :boolean false))))
  (is (= "nil" (step1/PRINT (types/mal-datum :nil nil))))
  )
