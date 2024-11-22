(ns step9-try-test
  (:require [clojure.test :refer [testing deftest is]]
            [core]
            [matcher-combinators.test]
            [types]
            [step9-try :as step9]))

(defn read-one-form [x & [env]]
  (let [env (or env (step9/gen-env core/built-in-env))
        _ (step9/extend-env env)
        [_reader form] (step9/READ x)]
    [form env]))

(defn read-eval-one-form [x & [env]]
  (let [[form env] (read-one-form x env)]
    [(step9/EVAL form env)
     env]))

(deftest try*-test
  (let [result (-> (try (read-eval-one-form "(try* boof)")
                        (catch clojure.lang.ExceptionInfo e
                          e))
                   ex-data
                   )]
    (is (= {:cause :uncaught-exception,
            :exception (types/mal-datum :exception
                                        (types/mal-datum :string "'boof' not found"))}
           result))))

(deftest throw-test
  (let [[result _env] (read-eval-one-form "(try* (throw \"yeep\") (catch x {:x x}))")]
    (is (= (types/mal-datum :map
                            {(types/mal-datum :keyword :x)
                             (types/mal-datum :exception
                                              (types/mal-datum :string "yeep"))})
           result)))

  (let [[result _env] (read-eval-one-form "(try* (map throw (list \"my err\")) (catch* exc exc))")]
    (is (= (types/mal-datum :exception (types/mal-datum :string "my err"))
           result))))

(deftest apply-test
  (let [[result _env] (read-eval-one-form "(apply symbol? (list (quote two)))")]
    (is (= (types/mal-datum :bool true)
           result)))
  (let [[result _env] (read-eval-one-form "(apply + 4 [5 6 7])")]
    (is (= (types/mal-datum :int 22)
           result))))

(deftest map-test
  (let [[result _env] (read-eval-one-form "(map (fn* (x) (symbol? x)) (list 1 (quote two) \"three\"))")]
    (is (= (types/mal-datum :list
                            [(types/mal-datum :bool false)
                             (types/mal-datum :bool true)
                             (types/mal-datum :bool false)])
           result)))

  (let [[result _env] (read-eval-one-form "(apply (fn* (& more) (list? more)) [1 2 3])")]
    (is (= (types/mal-datum :bool true)
           result))))
