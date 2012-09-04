(ns mc.sequence)

(defn slow-seq [delay-ms coll]
  "Returns a lazy sequence with time delays inserted between each element of the source collection." 
  (lazy-seq 
    (if-let [s (seq coll)]
        (do 
          (Thread/sleep delay-ms)
          (cons (first s)
                (slow-seq delay-ms (rest s)))))))