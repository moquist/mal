(ns types-test
  (:require [clojure.test :refer [testing is deftest]]
            [printer]
            [types]))

(deftest mal-print-string
  (testing "int"
    (is (= (printer/mal-print-string (types/->MalDatum :int 7) false)
           (printer/mal-print-string (types/->MalDatum :int 7) true)
           "7")))
  (testing "nil"
    (is (= (printer/mal-print-string (types/->MalDatum :nil nil) false)
           (printer/mal-print-string (types/->MalDatum :nil nil) true)
           "nil")))
  (testing "keyword"
    (is (= (printer/mal-print-string (types/->MalDatum :keyword :7) false)
           (printer/mal-print-string (types/->MalDatum :keyword :7) true)
           ":7")))
  (testing "string"
    (is (= (printer/mal-print-string (types/->MalDatum :string "seven") false)
           "seven"))
    (is (= (printer/mal-print-string (types/->MalDatum :string "seven") true)
           "\"seven\"")))
  (testing "bool"
    (is (= (printer/mal-print-string (types/->MalDatum :bool true) false)
           (printer/mal-print-string (types/->MalDatum :bool true) true)
           "true"))
    (is (= (printer/mal-print-string (types/->MalDatum :bool false) false)
           (printer/mal-print-string (types/->MalDatum :bool false) true)
           "false")))

  (testing "list"
    (testing "empty"
      (is (= (printer/mal-print-string (types/->MalDatum :list []) true)
             (printer/mal-print-string (types/->MalDatum :list []) false)
             "()")))
    (testing "not-empty"
      (is (= (printer/mal-print-string
               (types/->MalDatum :list [#types.MalDatum{:typ :int :datum-val 7}]) true)
             "(7)"))
      (is (= (printer/mal-print-string
               (types/->MalDatum :list [#types.MalDatum{:typ :int :datum-val 7}
                                        #types.MalDatum{:typ :keyword :datum-val :7}
                                        #types.MalDatum{:typ :bool :datum-val false}
                                        #types.MalDatum{:typ :nil :datum-val nil}
                                        #types.MalDatum{:typ :string :datum-val "hi"}])
               true)
             "(7 :7 false nil \"hi\")"))))
  (testing "vector"
    (testing "empty"
      (is (= (printer/mal-print-string (types/->MalDatum :vector []) true)
             (printer/mal-print-string (types/->MalDatum :vector []) false)
             "[]"))))
  (testing "map"
    (testing "empty"
      (is (= (printer/mal-print-string (types/->MalDatum :map {}) true)
             (printer/mal-print-string (types/->MalDatum :map {}) false)
             "{}")))
    (testing "not-empty"
      (is (= (printer/mal-print-string (types/->MalDatum :map {}) true)
             (printer/mal-print-string (types/->MalDatum :map {}) false)
             "{}"))
      (is (= (printer/mal-print-string
               (types/->MalDatum
                 :map
                 {#types.MalDatum{:typ :keyword :datum-val :a}
                  #types.MalDatum{:typ :string :datum-val "one"}

                  #types.MalDatum{:typ :bool :datum-val true}
                  #types.MalDatum{:typ :bool :datum-val false}})
               true)
             "{:a \"one\" true false}"))
      )
    )
  )
