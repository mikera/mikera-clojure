(ns test.combinations
  (:use [test.imports]))

(defn subsets [n items]
  "Get all subsets of size n from a list of items"
  (cond 
    (= n 0)
      '(())
    (empty? items)
      '()
    :default
      (concat
	      (map #(cons (first items) %) (subsets (dec n) (rest items)))
	      (subsets n (rest items)))))

(defn permutations [items]
  "Get all permutations of a list of items"
  (let [n (count items)]
    (if (= n 0)
      (list '())
	    (loop [i (dec n)
	           result '()]
	      (if (>= i 0)
	        (recur 
	          (dec i) 
	          (concat
	            (map
	              #(cons (nth items i) %)
	              (permutations (concat (take i items) (drop (inc i) items))))
	            result))
	        result)))))
	
(defn combinations [n items]
  "Get all possible (ordered) combinations of size n from items"
  (mapcat permutations (subsets n items)))

