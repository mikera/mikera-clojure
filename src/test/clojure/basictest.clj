(ns basictest
  (:use clojure.test))

(deftest test-arithmetic
  (testing "Basic arithmetic"
    (is (= 2 (+ 1 1)))))