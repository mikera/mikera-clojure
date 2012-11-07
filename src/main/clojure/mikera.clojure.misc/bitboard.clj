(ns test.bitboard)

(defn se [x y]
  "Produces a ray attack from the indexed bit in the south-east direction"
  (let [initial (bit-shift-left (bit-shift-left (long 1) x) (* y 8))
        dx 1 ;; x direction
        dy 1 ;; y direction
        distance (min 
                   (- 7 x) 
                   (- 7 y))
        shift (+ dx (* 8 dy))]
    (loop [value 0
           distance distance]
      (if (<= distance 0)
        value
        (recur (bit-or value (bit-shift-left initial (* distance shift))) (dec distance))))))

(defn bits [^long bitboard]
  (map 
    #(if (> (bit-and 1 (bit-shift-right bitboard %)) 0) 1 0)
    (range 64)))

(defn display [bitboard]
  (let [bits (partition 8 (bits bitboard))]
    (doseq [ss bits]
      (println (apply str ss)))))