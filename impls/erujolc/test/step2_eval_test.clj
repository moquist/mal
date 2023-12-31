(ns step2-eval-test
  (:require [clojure.test :refer [deftest is testing]]
            [step2-eval :as step2]
            types))

(deftest eval-ast
  (let [env (step2/gen-env [['+ clojure.core/+]
                            ['* clojure.core/*]])]
    (testing "symbol lookup"
      ;; cannot check for equality of the inline fn from step2/gen-env-entry
      (is (= ((:datum-val (step2/eval-ast (types/->MalDatum :symbol '+) env)) 1 2 3 4)
             10)))
    (testing "default no change"
      (is (= (:datum-val (step2/eval-ast (types/->MalDatum :barnacle :meep) env))
             :meep)))
    (testing "eval list"
      (let [[f & args]
            (:datum-val
              (step2/eval-ast (types/->MalDatum
                                :list
                                [(types/->MalDatum :symbol '+)
                                 (types/->MalDatum :int 1)
                                 (types/->MalDatum
                                   :list
                                   [(types/->MalDatum :symbol '*)
                                    (types/->MalDatum :int 2)
                                    (types/->MalDatum :int 3)])])
                              env))]
        (is (= (apply (:datum-val f) (map :datum-val args)) 7))))
    (testing "eval vector"
      (is (= (step2/eval-ast (types/->MalDatum
                               :vector
                               [(types/->MalDatum :symbol '+)
                                (types/->MalDatum :int 1)
                                (types/->MalDatum
                                  :list
                                  [(types/->MalDatum :symbol '*)
                                   (types/->MalDatum :int 2)
                                   (types/->MalDatum :int 3)])])
                             env)
             (types/->MalDatum
               :vector
               [(types/->MalDatum :fn +)
                (types/->MalDatum :int 1)
                (types/->MalDatum :undetermined 6)]))))

    (testing "eval map"
      (is (= (step2/eval-ast (types/->MalDatum
                               :map
                               {(types/->MalDatum :symbol '+)
                                (types/->MalDatum :int 1)
                                (types/->MalDatum :keyword :hiya)
                                (types/->MalDatum
                                  :list
                                  [(types/->MalDatum :symbol '*)
                                   (types/->MalDatum :int 2)
                                   (types/->MalDatum :int 3)])})
                             env)
             (types/->MalDatum :map 
                               {(types/->MalDatum :fn +)
                                (types/->MalDatum :int 1)

                                (types/->MalDatum :keyword :hiya)
                                (types/->MalDatum :undetermined 6)}))))))
