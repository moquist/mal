(ns reader-test
  (:require [clojure.test :refer [deftest is testing]]
            reader
            types))

(deftest tokenize
  (is (= (reader/tokenize "(+ 1 2 3)")
         [["(" "("] ["+" "+"] [" 1" "1"] [" 2" "2"] [" 3" "3"] [")" ")"]]))
  (is (= (reader/tokenize "   (+    1 2    3)  ;   hoopla")
         [["   (" "("]
          ["+" "+"]
          ["    1" "1"]
          [" 2" "2"]
          ["    3" "3"]
          [")" ")"]
          ["  ;   hoopla" ";   hoopla"]])))

(deftest reader-test
  (let [mal-reader (-> "(a b c)" reader/tokenize (reader/->MalReader 0))]
    (is (= (reader/mal-peek mal-reader) "("))
    (is (= (-> mal-reader reader/mal-next second) "("))
    (testing "reader is immutable"
      (is (= (-> mal-reader
                 reader/mal-step
                 reader/mal-next
                 second)
             "a"))
      (is (= (reader/mal-peek mal-reader) "(")))))

(deftest mal-reader
  (let [mal-reader (-> "(a b c)" reader/tokenize (reader/->MalReader 0))]
    (is (= (reader/mal-peek mal-reader) "("))
    (testing "reader is immutable; stepping forward does not mutate the original object"
      (is (= (-> mal-reader
                 reader/mal-step
                 reader/mal-next
                 second)
             "a"))
      (is (= (reader/mal-peek mal-reader) "(")))))

(defn read-atom-helper [x]
  (-> x reader/tokenize (reader/->MalReader 0) reader/read-atom second))

(deftest read-atom
  (is (= (read-atom-helper "a") (types/->MalDatum :symbol 'a)))
  (is (= (read-atom-helper ":a") (types/->MalDatum :keyword :a)))
  (is (= (read-atom-helper "123") (types/->MalDatum :int 123)))
  (is (= (read-atom-helper "nil") (types/->MalDatum :nil nil)))
  (is (= (read-atom-helper "false") (types/->MalDatum :bool false)))
  (is (= (read-atom-helper "true") (types/->MalDatum :bool true)))
  (is (= (read-atom-helper "\"true\"") (types/->MalDatum :string "true")))
  )

(defn read-coll-helper [x]
  (-> x reader/tokenize (reader/->MalReader 0) reader/mal-step))

(deftest read-coll
  (testing "empty list"
    (is (= (second (reader/read-coll :list (read-coll-helper "()")))
           #types.MalDatum{:typ :list, :datum-val []})))
  (testing "list of string"
    (is (= (second (reader/read-coll :list (read-coll-helper "(\"booga\")")))
           #types.MalDatum{:typ :list,
                           :datum-val [#types.MalDatum{:typ :string, :datum-val "booga"}]})))
  (testing "list of int"
    (is (= (second (reader/read-coll :list (read-coll-helper "(1)")))
           #types.MalDatum{:typ :list,
                           :datum-val [#types.MalDatum{:typ :int, :datum-val 1}]})))
  (testing "list of keyword"
    (is (= (second (reader/read-coll :list (read-coll-helper "(:a)")))
           #types.MalDatum{:typ :list,
                           :datum-val [#types.MalDatum{:typ :keyword, :datum-val :a}]})))
  (testing "empty vector"
    (is (= (second (reader/read-coll :vector (read-coll-helper "[]")))
           #types.MalDatum{:typ :vector :datum-val []})))
  (testing "vector of list, vector"
    (is (= (second (reader/read-coll :vector (read-coll-helper "[() []]")))
           #types.MalDatum{:typ :vector
                           :datum-val [#types.MalDatum{:typ :list
                                                       :datum-val []}
                                       #types.MalDatum{:typ :vector
                                                       :datum-val []}]})))
  (testing "empty map"
    (is (= (second (reader/read-coll :map (read-coll-helper "{}")))
           #types.MalDatum{:typ :map :datum-val {}})))
  )
