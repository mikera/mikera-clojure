(ns test.misc)

(defn all-pairs [coll]
  (let [x (first coll)
        xs (rest coll)]
    (if (empty? xs) 
      nil
      (concat 
        (map (fn [y] [x y]) xs) 
        (all-pairs xs)))))

(def mystream (atom (range 100)))

(defn my-take [n stream]
  (let [data @stream
        result (take n data)]
    (reset! stream (drop n data))
    result))  
