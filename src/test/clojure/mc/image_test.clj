(ns mc.image-test
  (:use clojure.test)
  (:use [mc image]))

(deftest test-arithmetic
  (testing "Basic arithmetic"
    (is (= 2 (+ 1 1)))))