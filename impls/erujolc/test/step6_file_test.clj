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
           (types/->MalDatum :undetermined 5)))
    ))
