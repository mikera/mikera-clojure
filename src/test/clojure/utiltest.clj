(ns utiltest
  (:use clojure.test)
  (:use mc.util))

(deftest test-applying
  (testing "applyn"
    (is (= 810 (applyn inc 10 800)))
    (is (= `(inc (inc 6)) (macroexpand `(applyn inc 2 6))))))