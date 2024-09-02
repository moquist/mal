(ns step8-macros-test
  (:require [clojure.test :refer [testing deftest is]]
            [core]
            [types]
            [step8-macros :as step8]))

(defn read-one-form [x & [env]]
  (let [env (or env (step8/gen-env core/built-in-env))
        [_reader form] (step8/READ x)]
    [form env]))

(defn read-eval-one-form [x & [env]]
  (let [[form env] (read-one-form x env)]
    [(step8/EVAL form env)
     env]))

(deftest defmacro-test
  (let [[result env] (read-eval-one-form "(def! monkey 1)")]
    (is (= (-> env :data deref (get (types/mal-datum :symbol 'monkey)))
          (types/mal-datum :int 1))))
  (let [[result env] (read-eval-one-form "(def! monkey (fn* () 1))")]
    (is (false? (-> env
                    :data
                    deref
                    (get (types/mal-datum :symbol 'monkey))
                    :datum-val
                    :macro?))))
  (let [[result env] (read-eval-one-form "(defmacro! monkey (fn* () 1))")]
    (is (-> env
            :data
            deref
            (get (types/mal-datum :symbol 'monkey))
            :datum-val
            :macro?))))

(deftest ast->maybe-macro-call-test
  (let [[_result env] (read-eval-one-form "(defmacro! monkey (fn* () 1))")
        [ast env] (read-one-form "(monkey)" env)]
    (is (step8/ast->maybe-macro-call ast env)))
  (let [[ast env] (read-one-form "(+ 1 2)")]
    (is (not (step8/ast->maybe-macro-call ast env)))))


