(ns testmath
  (:use mc.math)
  (:use clojure.test))

(deftest test-integer-progression
  (testing "end values"
    (is (= 1 (first (integer-progression 1 10 5))))
    (is (= 10 (last (integer-progression 1 10 5))))))
