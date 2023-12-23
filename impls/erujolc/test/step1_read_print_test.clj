(ns step1-read-print-test
  (:require [clojure.test :refer [deftest testing is]]
            [step1-read-print :as step1]))

(deftest READ
  (is (= (second (step1/READ "")) :reader/peeked-into-the-abyss))
  (is (= (second (step1/READ "1")) #types.MalDatum{:typ :int :datum-val 1}))
  (is (= (second (step1/READ " 1 ")) #types.MalDatum{:typ :int :datum-val 1}))
  (is (= (second (step1/READ " 1  2")) #types.MalDatum{:typ :int :datum-val 1}))
  (is (= (second (step1/READ "()")) #types.MalDatum{:typ :list :datum-val []}))
  (is (= (second (step1/READ "(")) #types.MalDatum{:typ :list :datum-val []}))
  )
