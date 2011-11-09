(ns testio
  (:use [mc.resource])
  (:use clojure.test))

(deftest test-resource-file
  (is (not (nil? (mc.resource/resource "euler/names.txt")))))

