(ns mc.math)



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

(defn gcd [a b] (let [m (mod b a)] (if (pos? m) (gcd m a) a)))

;; (histogram 100 (fn [] (mikera.util.Rand/u)) 0.1)

;; testing distribution of the sigmoid of a gaussian
;; (histogram 10000 (fn [] (mikera.util.Maths/sigmoid (* (/ Math/PI 2) (mikera.util.Rand/nextGaussian)))) 0.1)