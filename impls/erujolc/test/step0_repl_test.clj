(ns step0-repl-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [step0-repl :as step0]))

(deftest rep-test
  (is (= (step0/rep 7) 7)))

(deftest prompt-test
  (with-open [rdr (io/reader (char-array "hiya"))]
    (binding [*in* rdr]
      (is (= (step0/prompt)) "hiya")))
  (with-open [rdr (io/reader (char-array ""))]
    (binding [*in* rdr]
      (is (= (step0/prompt)) "")))
  (with-open [rdr (io/reader (char-array "    booga    "))]
    (binding [*in* rdr]
      (is (= (step0/prompt)) "booga")))
  (with-open [rdr (io/reader (char-array "\n"))]
    (binding [*in* rdr]
      (is (= (step0/prompt)) "")))
  (with-redefs [clojure.core/read-line (constantly nil)]
    (is (nil? (step0/prompt))))
  (testing "handle correctly: EOD from console results in 'nil from .readLine on reader"
    (binding [*in* (io/reader (io/input-stream (.getBytes "")))]
      (is (nil? (step0/prompt))))))
