(ns testio
  (:use mc.resource)
  (:use clojure.test))

(deftest test-io
  (testing "Resource IO"
    (is (mc.resource/resource "euler/names.txt"))))
