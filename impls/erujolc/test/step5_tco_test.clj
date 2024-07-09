(ns step5-tco-test
  (:require [clojure.test :refer [testing is deftest]]
            [core]
            [step5-tco :as step5]))

(deftest tco-test
  (let [env (step5/gen-env core/built-in-env)
        [_reader form] (step5/READ "(def! res2 (sum2 10 0))")
        [_reader form2] (step5/READ "(def! res2 (sum2 10000 0))")]
    (step5/rep
      "(def! sum2
         (fn* (n acc)
           (if (= n 0)
             acc
             (sum2 (- n 1) (+ n acc)))))"
      env)
    (is (= (step5/EVAL form env)
           (types/->MalDatum :undetermined 55)))
    (is (= (step5/EVAL form2 env)
           (types/->MalDatum :undetermined 50005000)))))


(deftest mutual-recursion-test
  (let [env (step5/gen-env core/built-in-env)
        [_reader form] (step5/READ "(foo 10000)")]
    (step5/rep "(def! foo (fn* (n) (if (= n 0) 0 (bar (- n 1)))))" env)
    (step5/rep "(def! bar (fn* (n) (if (= n 0) 0 (foo (- n 1)))))" env)
    (is (= (step5/EVAL form env)
           (types/->MalDatum :int 0)))))
