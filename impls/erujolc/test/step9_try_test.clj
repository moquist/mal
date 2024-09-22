(ns step9-try-test
  (:require [clojure.test :refer [testing deftest is]]
            [core]
            [matcher-combinators.test]
            [types]
            [step9-try :as step9]))

(defn read-one-form [x & [env]]
  (let [env (or env (step9/gen-env core/built-in-env))
        [_reader form] (step9/READ x)]
    [form env]))

(defn read-eval-one-form [x & [env]]
  (let [[form env] (read-one-form x env)]
    [(step9/EVAL form env)
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
    (is (step9/ast->maybe-macro-call ast env)))
  (let [[ast env] (read-one-form "(+ 1 2)")]
    (is (not (step9/ast->maybe-macro-call ast env)))))

(deftest mal-macroexpand-test
  (let [[result env] (read-eval-one-form
                       "(defmacro! unless (fn* (pred a b) (quasiquote (if ~pred ~b ~a))))")]

    (testing "basic macroexpand"
      (let [[result2 env] (read-eval-one-form "(macroexpand (unless PRED :A :B))" env)]
        (is (= #types.MalDatum{:typ :list,
                               :datum-val [#types.MalDatum{:typ :symbol, :datum-val if}
                                           #types.MalDatum{:typ :symbol, :datum-val PRED}
                                           #types.MalDatum{:typ :keyword, :datum-val :B}
                                           #types.MalDatum{:typ :keyword, :datum-val :A}]}
               result2))))

    (testing "basic macro"
      (let [e (ex-data (try (read-eval-one-form "(unless PRED :A :B)" env)
                            (catch clojure.lang.ExceptionInfo e
                              e)))]
        (is (match? {:cause :ns-resolve-failed
                     :key (types/mal-datum :symbol 'PRED)}
                    e))))

    (testing "basic macro"
      (let [[_ env] (read-eval-one-form "(def! PRED false)" env)
            [result3 env] (read-eval-one-form "(unless PRED :A :B)" env)]
        (is (= (types/mal-datum :keyword :A)
               result3))))))
