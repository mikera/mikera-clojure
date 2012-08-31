(ns test.arrow)

(def plus2 #(+ %1 2))


;; arrow combinators

; create arrow
(defn arr [f]
	(fn [] f))

; sequence arrows
(defn sq 
  ([a b] 
		(arr (fn [v] ((b) ((a) v)))))
	([a b & more] 
		(reduce sq (sq a b) more)))


   
 
(def x
  (arr plus2))

(defn a-add [v]
  (arr 
		(fn [x] (+ x v))))


