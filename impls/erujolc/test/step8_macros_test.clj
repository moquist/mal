(ns step8-macros-test
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

(deftest concat-test
  (let [env (step7/gen-env core/built-in-env)
        [_reader form] (step7/READ "(concat (list 4 5 6) (list 1 2 3) (list 7 8 9))")
        ]
    (is (= (step7/EVAL form env)
           (types/mal-datum :list
                            [(types/mal-datum :int 4)
                             (types/mal-datum :int 5)
                             (types/mal-datum :int 6)
                             (types/mal-datum :int 1)
                             (types/mal-datum :int 2)
                             (types/mal-datum :int 3)
                             (types/mal-datum :int 7)
                             (types/mal-datum :int 8)
                             (types/mal-datum :int 9)])))))

(deftest quote-test
  (let [env (step7/gen-env core/built-in-env)
        [_reader form] (step7/READ "(quote (list 1 2 3))")]
    (is (= (step7/EVAL form env)
           (types/mal-datum :list
                            [(types/mal-datum :symbol 'list)
                             (types/mal-datum :int 1)
                             (types/mal-datum :int 2)
                             (types/mal-datum :int 3)])))))

(deftest quasiquote-test
  (let [env (step7/gen-env core/built-in-env)
        cases [{:description "basic quasiquote"
                :input "(quasiquote (list 1 2 3))"
                :expected (types/mal-datum :list
                                           [(types/mal-datum :symbol 'list)
                                            (types/mal-datum :int 1)
                                            (types/mal-datum :int 2)
                                            (types/mal-datum :int 3)])}
               {:description "quasiquote with unquote"
                :input "(quasiquote (unquote (list 1 2 3)))"
                :expected (types/mal-datum :list
                                           [(types/mal-datum :int 1)
                                            (types/mal-datum :int 2)
                                            (types/mal-datum :int 3)])}

               {:description "quasiquote with nested unquote"
                :input "(quasiquote (list \"meep\" (unquote (list 1 2 3))))"
                :expected (types/mal-datum
                            :list
                            [(types/mal-datum :symbol 'list)
                             (types/mal-datum :string "meep")
                             (types/mal-datum
                               :list
                               [(types/mal-datum :int 1)
                                (types/mal-datum :int 2)
                                (types/mal-datum :int 3)])])}

               {:description "quasiquote with splice-unquote and an arbitrary symbol"
                :input "(quasiquote (beegle \"meep\" (splice-unquote (list 1 2 3))))"
                :expected (types/mal-datum
                            :list
                            [(types/mal-datum :symbol 'beegle)
                             (types/mal-datum :string "meep")
                             (types/mal-datum :int 1)
                             (types/mal-datum :int 2)
                             (types/mal-datum :int 3)])}

               {:description "quasiquote with splice-unquote and a map"
                :input "(quasiquote (beegle {:a 1} (splice-unquote (list 1 2 3))))"
                :expected (types/mal-datum
                            :list
                            [(types/mal-datum :symbol 'beegle)
                             (types/mal-datum :map {(types/mal-datum :keyword :a)
                                                    (types/mal-datum :int 1)})
                             (types/mal-datum :int 1)
                             (types/mal-datum :int 2)
                             (types/mal-datum :int 3)])}
               ]]
    (doseq [{:keys [description input expected]} cases
            :let [[_reader form] (step7/READ input)
                  result (step7/EVAL form env)]]
      (testing description
        (is (= result expected))))))

(deftest eval-vs-eval-ast
  "I keep getting confused which one I want.
  Maybe I have bugs (or just needlessly complex code) because of this confusion.
  Sort it out with a test."
  (testing "single fn-call list"
    (let [env (step7/gen-env core/built-in-env)
          [_reader form] (step7/READ "(+ 1 2)")
          mal-data (types/mal-datum :list
                                    [(types/mal-datum :symbol '+)
                                     (types/mal-datum :int 1)
                                     (types/mal-datum :int 2)])
          [_reader mal-symbol-+] (step7/READ "+")
          mal-hostfn-+ (step7/eval-ast mal-symbol-+ env)]
      (testing "reader returns literal mal data"
        (is (= form mal-data)))
      (testing "the mal hostfn is a clojure function"
        (is (fn? (:datum-val mal-hostfn-+))))
      (testing "eval-ast evals everything except the top-level form"
        (is (= (step7/eval-ast mal-data env)
               (assoc-in mal-data [:datum-val 0] mal-hostfn-+))))
      (testing "EVAL calls the fn"
        (is (= (step7/EVAL mal-data env)
               (types/mal-datum :undetermined 3))))))

  (testing "nested fn-call"
    (let [env (step7/gen-env core/built-in-env)
          [_reader form] (step7/READ "(+ 1 2 (+ 5 7))")
          mal-data (types/mal-datum :list
                                    [(types/mal-datum :symbol '+)
                                     (types/mal-datum :int 1)
                                     (types/mal-datum :int 2)
                                     (types/mal-datum :list
                                                      [(types/mal-datum :symbol '+)
                                                       (types/mal-datum :int 5)
                                                       (types/mal-datum :int 7)])])
          [_reader mal-symbol-+] (step7/READ "+")
          mal-hostfn-+ (step7/eval-ast mal-symbol-+ env)]
      (testing "reader returns literal mal data"
        (is (= form mal-data)))
      (testing "eval-ast evals everything but the top-level form"
        (is (= (step7/eval-ast mal-data env)
               (-> mal-data
                   (assoc-in [:datum-val 0] mal-hostfn-+)
                   (assoc-in [:datum-val 3] (types/mal-datum :undetermined 12))))))
      (testing "EVAL evals the top-level form"
        (is (= (step7/EVAL mal-data env)
               (types/mal-datum :undetermined 15))))))
  )





