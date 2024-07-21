(ns step6-file-test
  (:require [clojure.test :refer [deftest testing is]]
            [core]
            [step6-file :as step6]))

(deftest eval-test
  (let [env (step6/gen-env core/built-in-env)
        [_reader form] (step6/READ "(eval mal-prog)")
        [_reader form2] (step6/READ "(eval (list + 2 3))")
        ]
    (step6/rep "(def! mal-prog (list + 1 2))" env)
    (is (= (step6/EVAL form env)
           (types/->MalDatum :undetermined 3)))
    (is (= (step6/EVAL form2 env)
           (types/->MalDatum :undetermined 5))))
  (testing "eval doesn't use local env"
    (let [env (step6/gen-env core/built-in-env)
          [_reader form] (step6/READ "(let* (a 1)
                                        (eval (read-string \"a\")))")]
      (step6/rep "(def! a 7)" env)
      (is (= (step6/EVAL form env)
             (types/->MalDatum :int 7))))))

#_ ; dup, but used for debugging and comprehension
(deftest fncall-test
  (let [env (step6/gen-env core/built-in-env)]
    (is (= (step6/EVAL
             (types/mal-datum :list [(types/mal-datum :symbol '+)
                                     (types/mal-datum :int 1)
                                     (types/mal-datum :int 2)])
             env)
           (types/mal-datum :undetermined 3)))))

(deftest swap!-test
  (let [env (step6/gen-env core/built-in-env)]
    (step6/rep "(def! mef (atom 7))" env)
    (step6/rep "(def! inc (fn* (n) (+ n 1)))" env)
    (testing "atom initialized as expected"
      (is (= (step6/EVAL
               (types/mal-datum :list [(types/mal-datum :symbol 'deref)
                                       (types/mal-datum :symbol 'mef)])
               env)
             (types/mal-datum :int 7))))
    (is (= (step6/EVAL
             (types/mal-datum :list [(types/mal-datum :symbol 'swap!)
                                     (types/mal-datum :symbol 'mef)
                                     (types/mal-datum :symbol 'inc)])
             env)
           (types/mal-datum :undetermined 8)))
    (testing "atom state has changed"
      (is (= (step6/EVAL
               (types/mal-datum :list [(types/mal-datum :symbol 'deref)
                                       (types/mal-datum :symbol 'mef)])
               env)
             (types/mal-datum :undetermined 8))))))
