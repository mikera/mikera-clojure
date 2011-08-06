(ns mc.util
  (:import mikera.util.Rand)
  (:import [mikera.util Tools])
  (:require [clojure.set])
  (:use [clojure.test]))

(defn debug [object]
  (mikera.util.Tools/debugBreak object))

(defn run-at-intervals [f interval-ms ms]
  "Sets off a thread with a function that will be run at intervals for the given time period"
  (future
	  (let [start-time (System/currentTimeMillis)
	        end-time (+ start-time ms)
	        loop-count (atom 0)]
	    (while (< (System/currentTimeMillis) end-time) (do
	      (Thread/sleep interval-ms)
	      (f))))))

(defn run-loop [f ms]
  "Runs a function repeatedly in a loop for a given number of millisenonds"
  (let [start-time (System/currentTimeMillis)
        end-time (+ start-time ms)
        loop-count (atom 0)]
    (while (< (System/currentTimeMillis) end-time) (do
      (f)
      (swap! loop-count inc)))
    {:time ms 
     :iterations @loop-count 
     :rate (double (/ (* @loop-count 1000) ms))}))

(defn find-first [pred coll]
  "Searches a collection and returns the first item for which pred is true, nil if not found"
  (if (empty? coll) nil
    (let [v (first coll)]
      (if (pred v)
        v
        (recur pred (rest coll))))))

(defn find-position 
  "Searches a collection and returns the index of the item's position"
  ([coll item] 
    (find-position coll item 0))
  ([coll item i] 
    (if (empty? coll) 
      nil
	    (let [v (first coll)]
	      (if (= item v)
	        i
	        (recur (rest coll) item (inc i)))))))

(defn middle [x y z]
  (if (> x y)
    (if (<= x z) 
      x
      (max y z))
    (if (<= y z) 
      y
      (max x z))))

(defn reduce-indexed 
  "Reduce while adding an index as the second argument to the function"
  ([f coll]
    (reduce-indexed f (first coll) 0 (rest coll)))

  ([f init coll]
    (reduce-indexed f init 0 coll))

  ([f init i coll]
    (if (empty? coll)
      init
      (let [v (first coll)
            fv (f init i v)]
        (recur f fv (inc i) (rest coll))))))

(defn map-vector 
  "Applies a function f to every value of a vector v as in map, non-lazy"
  ([f v]
    (map-vector f v []))
  ([f v result]
    (if (empty? v)
      result
      (recur f (rest v) (conj result (f (first v)))))))

(defn remove-nth [avector n]
  (vec
    (concat
      (subvec avector 0 n) (subvec avector (inc n) (count avector)))))

  
(defn remove-nils [[x & xs]]
  (let [rest (if (empty? xs) '() (remove-nils xs))]
    (if (nil? x)
      rest
      (cons x rest))))


(defn list-not-nil [& xs]
  (remove-nils xs))

(defmacro applyn [f n x] 
    (cond 
        (<= n 0) x
        true `(let [f# ~f] (applyn f# ~(dec n) (f# ~x)))    
        )
      )

(defmacro repeated-apply [f n] 
    (cond 
        (<= n 0) identity
        true `(let [f# ~f] (fn [a#] ((repeated-apply f# ~(dec n)) a#))    
        )
      ))

(defn get-element-after [coll value]
  (if (= value (first coll))
    (first (rest coll))
    (recur (rest coll) value)))

(defn list-contains? [coll value]
  (let [s (seq coll)]
	  (if s
      (if (= (first s) value) true (recur (rest s) value))
	    false)))

(defn map-difference2 [m1 m2]
  "Gets the difference between two maps"
  (loop [m (transient {})
         ks (concat (keys m1) (keys m2))]
    (if-let [k (first ks)]
      (let [e1 (get m1 k)
            e2 (get m2 k)]
        (cond 
          (= e1 e2) 
            (recur m (rest ks))
          (not e1) 
            (recur (assoc! m k nil) (rest ks))
          :else    
            (recur (assoc! m k e1) (rest ks))))
      (persistent! m))))

(defn map-difference3 [m1 m2]
  "Gets the difference between two maps"
  (if (nil? m2)
    m1
	  (let [md (mikera.util.Tools/mapDifference m1 m2)]
	    (merge {} md))))

(defn map-difference [m1 m2]
  "Gets the difference between two maps"
  (loop [m {}
         ks (concat (keys m1) (keys m2))]
    (if-let [k (first ks)]
      (let [v1 (get m1 k)
            v2 (get m2 k)]
        (cond 
          (= v1 v2) 
            (recur m (rest ks))
          (not v1) 
            (recur (assoc m k nil) (rest ks))
          :else    
            (recur (assoc m k v1) (rest ks))))
      m)))

; (time (dotimes [i 1000] (map-difference (:game @state) nil)))

(defn rand-choice [s]
  (nth s (Rand/r (count s))))

(defn argmax 
  ([f items]
    (let [v (first items)] 
      (argmax f (rest items) (f v) v )))
  ([f items bestvalue best]
    (if (empty? items)
      best
      (let [v (first items)
            fv (f v)] 
	      (if (> fv bestvalue)
	        (recur f (rest items) fv v)
	        (recur f (rest items) bestvalue best))))))

(defn valmax 
  ([f items]
    (let [v (first items)] 
      (valmax f (rest items) (f v) v )))
  ([f items bestvalue best]
    (if (empty? items)
      bestvalue
      (let [v (first items)
            fv (f v)] 
        (if (> fv bestvalue)
          (recur f (rest items) fv v)
          (recur f (rest items) bestvalue best))))))

(deftest test-middle
  (is (= 2 (middle 1 2 3)))
  (is (= 2 (middle 3 1 2)))
  (is (= 2 (middle 2 3 1))))

(deftest test-map-difference
  (is (= {} (map-difference {:a 1 :b 2} {:a 1 :b 2})))
  (is (= {:a 1} (map-difference {:a 1 :b 2} {:b 2})))
  (is (= {:a 1 :b 2} (map-difference {:a 1 :b 2} {})))
  (is (= {:a 1 :b nil} (map-difference {:a 1} {:b 2}))))

(deftest test-list-not-nil
  (is (= '() (list-not-nil nil nil)))
  (is (= '() (list-not-nil nil)))
  (is (= '(1 2 3 4 5) (list-not-nil nil 1 nil 2 3 4 5))))

(deftest test-find-position
  (is (= nil (find-position nil "fvfvf")))
  (is (= 2 (find-position [1 2 3 4] 3)))
  (is (= nil (find-position [1 2 3 4] 5))))


(deftest test-list-contains
  (is (list-contains? [1 2 3 4 5] 4))
  (is (list-contains? '(1 2 3 4 5) 4))
  (is (not (list-contains? '(1 2 3 4 5) 0)))
  (is (not (list-contains? nil 0))))

(deftest test-argmax
  (let [f (fn [x] (* x x))]
    (is (= 2 (argmax inc [2])))
    (is (= -10 (argmax f [-10 4 6])))))

(deftest test-valmax
  (let [f (fn [x] (* x x))]
    (is (= 3 (valmax inc [2])))
    (is (= 7 (valmax inc [-10 4 6])))))
