(ns mc.util
  (:import mikera.util.Rand))

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

(defn rand-choice [s]
  (nth s (Rand/r (count s))))