(ns testio
  (:use mc.resource)
  (:use clojure.test))

(deftest test-arithmetic
  (testing "Basic arithmetic"
    (is (mc.resource/resource "euler/names.txt"))))
