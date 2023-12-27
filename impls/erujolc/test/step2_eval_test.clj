(ns step2-eval-test
  (:require [clojure.test :refer [deftest is testing]]
            [step2-eval :as step2]
            types))

(deftest eval-ast
  (let [env (step2/gen-env [['+ clojure.core/+]
                            ['* clojure.core/*]])]
    (testing "symbol lookup"
      ;; cannot check for equality of the inline fn from step2/gen-env-entry
      (is (= ((step2/eval-ast (types/->MalDatum :symbol '+) env) 1 2 3 4)
             10)))
    (testing "default no change"
      (is (= (step2/eval-ast (types/->MalDatum :barnacle :meep) env)
             :meep)))
    (testing "eval list"
      (let [[f & args]
            (step2/eval-ast (types/->MalDatum
                              :list
                              [(types/->MalDatum :symbol '+)
                               (types/->MalDatum :int 1)
                               (types/->MalDatum
                                 :list
                                 [(types/->MalDatum :symbol '*)
                                  (types/->MalDatum :int 2)
                                  (types/->MalDatum :int 3)])])
                            env)]
        (is (= (apply f args) 7))))))
