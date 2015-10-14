(defproject erujolc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :step0 {:main ^:skip-aot step0-repl}
             :step1 {:main ^:skip-aot step1-read-print}
             :step2 {:main ^:skip-aot step2-eval}
             :step3 {:main ^:skip-aot step3-env}
             :step4 {:main ^:skip-aot step4-if-fn-do}
             })
