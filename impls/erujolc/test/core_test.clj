(ns core-test
  (:require [clojure.test :refer [deftest is testing]]
            core
            types))

(deftest mal-pr-str
  (is (= (core/mal-pr-str (types/->MalDatum :int 7))
         (types/->MalDatum :string "7")))
  )
