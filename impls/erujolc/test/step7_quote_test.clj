(ns step7-quote-test
  (:require [clojure.test :refer [testing deftest is]]
            [core]
            [types]
            [step7-quote :as step7]))

(deftest cons-test
  (let [env (step7/gen-env core/built-in-env)
        [_reader form] (step7/READ "(cons 7 (list 1 2 3))")
        ]
    (is (= (step7/EVAL form env)
           (types/mal-datum :list
                            [(types/mal-datum :int 7)
                             (types/mal-datum :int 1)
                             (types/mal-datum :int 2)
                             (types/mal-datum :int 3)])))))
