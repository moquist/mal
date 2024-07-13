(ns env-test
  (:require [clojure.test :refer [testing deftest is]]
            env
            types))

(deftest env
  (let [e (env/mal-environer nil [:a :b] [1 2])]
    (is (= (env/get e :a) 1)))
  (let [e (env/mal-environer (env/mal-environer nil [:a :b] [1 2])
                             [:a :c] [3 4])]
    (testing "env"
      (is (= (env/get e :c) 4)))
    (testing "env priority over outer"
      (is (= (env/get e :a) 3)))
    (testing "recursing to outer"
      (is (= (env/get e :b) 2)))
    (testing "set"
      (let [[e _] (env/set e :g "whiz")]
        (is (= (env/get e :g) "whiz")))
      #_
      (testing "env immutable"
        (is (thrown-with-msg? 
              clojure.lang.ExceptionInfo
              #"not found"
              (= (env/get e :g) "whiz"))))
      (testing "env NOT immutable"
        (is (= (env/get e :g) "whiz"))))
    (testing "def"
      (let [e2 (env/mal-environer e [:x :y :z] [9 8 7])
            [e3 _] (env/def e2 :h "whiz")]
        (is (= (env/get e3 :h) "whiz"))
        (testing "def does not def in nested (non-outermost) env"
          (is (-> e3 :data deref :h nil?))))
      #_
      (testing "env immutable"
        (is (thrown-with-msg?
              clojure.lang.ExceptionInfo
              #"not found"
              (= (env/get e :g) "whiz"))))
      (testing "env NOT immutable"
        (is (= (env/get e :g) "whiz"))))
    ))

(deftest handle-variadic
  (is (= (env/handle-variadic [:a :b :c] [1 2 3])
         {:a 1 :b 2 :c 3}))
  (is (= (env/handle-variadic
           [:a (types/->MalDatum :symbol '&) :b]
           [1 2 3])
         {:a 1
          :b (types/->MalDatum :list [2 3])})))
