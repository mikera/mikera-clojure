(ns test.evolve.evolve
	(:use [clojure.tools.macro])) 


(def create (fn [] [(rand)]))

(def mutate (fn [v] 
	(let [c (count v)]
		(if (= c 0)
			('create)
			(assoc v (rand-int (inc (count v))) (rand))))))

(def popcount 10)

(def initial (vec (map (fn [_] (create)) (range popcount))))

(def current initial)

(def fit #(first %))

(defn step [vs]
	(let [
		sort_vs (rseq (vec (sort-by fit vs))) 
		n (count vs)
		r (quot n 2)
		winners (take r sort_vs)]
		(concat winners (map mutate (repeat (- n r) (nth winners (rand-int r))))))) 

(defn best [] (first current)) 

(defn evolve [steps] 
	(dotimes [n steps]
		(do
			(def current (step current))
			(println (str (best))))))
