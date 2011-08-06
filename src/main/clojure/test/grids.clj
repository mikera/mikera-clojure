(ns test.grids)

(defn make-row [w]
  (vec (for [x (range w)] 0)))

(defn make-grid [w h]
  (vec (for [y (range h)] (make-row w))))

(defn gget [grid x y]
  ((grid y) x))

(defn gset [grid x y v]
  (assoc grid y (assoc (grid y) x v)))