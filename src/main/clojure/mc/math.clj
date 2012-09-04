(ns mc.math
  (:use mc.util)
  (:import [java.lang.Math]))



(defn histogram [n some-function gap]
  (let [floats (sort (take n (repeatedly some-function)))]
    (->
	    (reduce
	      (fn [hist val] 
	        (let [r (* (int (/ val gap)) gap)
	              n (or (hist r) 0)]
	          (assoc hist r (inc n))))
	      {}
	      floats)
      sort)))

(defn gcd 
  "Returns the greatest common divisor of two numbers" 
  ([a b] 
    (let [m (mod b a)] 
      (if (pos? m) (gcd m a) a))))

(defn integer-progression 
  "Create a sequence of (possibly not unique) integers approximating an arithmetic progression from start to end that is exactly count long"
  ([start end ^long count]
    (let [increment (/ (- (double end) (double start)) (dec count))]
      (for [i (range count)]
        (java.lang.Math/round (double (+ start (* increment (double i)))))))))

;; (histogram 100 (fn [] (mikera.util.Rand/u)) 0.1)

;; testing distribution of the sigmoid of a gaussian
;; (histogram 10000 (fn [] (mikera.util.Maths/sigmoid (* (/ Math/PI 2) (mikera.util.Rand/nextGaussian)))) 0.1)