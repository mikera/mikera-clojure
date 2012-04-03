(ns testserial
  (:use clojure.test)
  (:use [mc serial]))

(defrecord A [])

(defn rebuild [x]
  (deserialize (serialize x)))

(defn test-rebuild [x]
  (is (= x (rebuild x))))

(deftest test-serializations
  (testing "Rebuilding data"
    (test-rebuild 2)
    (test-rebuild {:a 1 :b 2})
    (test-rebuild (A.))))