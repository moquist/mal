(ns env-test
  (:require [clojure.test :refer [testing deftest is]]
            env))

(deftest env
  (let [e (env/mal-environer nil {:a 1 :b 2})]
    (is (= (env/get e :a) 1)))
  (let [e (env/mal-environer (env/mal-environer nil {:a 1 :b 2})
                             {:a 3 :c 4})]
    (testing "env"
      (is (= (env/get e :c) 4)))
    (testing "env priority over outer"
      (is (= (env/get e :a) 3)))
    (testing "recursing to outer"
      (is (= (env/get e :b) 2)))
    (testing "set"
      (let [e (env/set e :g "whiz")]
        (is (= (env/get e :g) "whiz")))
      (testing "env immutable"
        (is (thrown-with-msg? 
              clojure.lang.ExceptionInfo
              #"not found"
              (= (env/get e :g) "whiz")))))))
