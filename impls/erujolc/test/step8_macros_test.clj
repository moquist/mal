(ns step8-macros-test
  (:require [clojure.test :refer [testing deftest is]]
            [core]
            [matcher-combinators.test]
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

(deftest mal-macroexpand-test
  (let [[_result env] (read-eval-one-form
                        "(defmacro! unless (fn* (pred a b) (quasiquote (if ~pred ~b ~a))))")]

    (testing "basic macroexpand"
      (let [[result2 _env] (read-eval-one-form "(macroexpand (unless PRED :A :B))" env)]
        (is (= #types.MalDatum{:typ :list,
                               :datum-val [#types.MalDatum{:typ :symbol, :datum-val if}
                                           #types.MalDatum{:typ :symbol, :datum-val PRED}
                                           #types.MalDatum{:typ :keyword, :datum-val :B}
                                           #types.MalDatum{:typ :keyword, :datum-val :A}]}
               result2))))

    (testing "basic macro with exception"
      (let [e (-> (try (read-eval-one-form "(unless PRED :A :B)" env)
                       (catch clojure.lang.ExceptionInfo e
                         e))
                  ex-data
                  (select-keys [:cause :symbol]))]
        (is (= e {:cause :undefined-symbol, :symbol (types/mal-datum :symbol 'PRED)}))))

    (testing "basic macro"
      (let [[_ env] (read-eval-one-form "(def! PRED false)" env)
            [result3 env] (read-eval-one-form "(unless PRED :A :B)" env)]
        (is (= (types/mal-datum :keyword :A)
               result3))))))
