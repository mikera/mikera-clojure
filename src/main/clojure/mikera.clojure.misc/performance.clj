(ns test.performance
  (:import [mikera.stats PerformanceTest]))

(set! *warn-on-reflection* true)

(defn a [] (reduce + (range 0 1000000)))

(do (dotimes [i 10] (a)) (time (a)))

(defn b [] (loop [acc (int 0) i (int 0)] (if (>= i (int 1000000)) acc (recur (unchecked-add acc i) (unchecked-inc i)) )))

(do (dotimes [i 10] (b)) (time (b)))

(defn c [] (PerformanceTest/addMillion))

(do (dotimes [i 10] (c)) (time (c)))

