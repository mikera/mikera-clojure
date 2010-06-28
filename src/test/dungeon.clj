(ns test.dungeon
	(:use [clojure.contrib.str-utils])
)

(set! *warn-on-reflection* true)

(def directions [[0 -1] [-1 0] [1 0] [0 1]])

(def directions8 [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]])


(defn add-dir [[x y] [dx dy]]
	[(+ x dx) (+ y dy)]
)

(defn sub-dir [[x y] [dx dy]]
	[(- x dx) (- y dy)]
)

(defn make-map [a w h]
	(reduce 
		#(assoc %1 %2 a)
		{}
		(for [y (range h) x (range w)]  
			[x y]
		)
	)
)

(defn paste [dest src]
	(reduce
		#(assoc %1 (first %2) (second %2))
		dest
		src
	)
)

(defn offset [src [dx dy]]
	(reduce
		#(assoc %1 (add-dir (first %2) [dx dy]) (second %2))
		{}  
		src
	)
)
 
(defn subset [a b]
	(every? 
		#(= (b (first %1)) (second %1))
		a
	)
)
 
(defn transform-coords [f src]
	(reduce
		#(assoc %1 (f (first %2)) (second %2))
		{}  
		src
	)	
)



(defn neighbours [p]
	(map #(add-dir p %) directions)
)

(def pi-factor (double (* Math/PI (float 0.5))))
 
(defn rotate-loc [#^Integer quarter-turns [#^Integer x #^Integer y]]
			[
				(int (Math/round (+ (* (double x) (Math/cos (* (double quarter-turns) pi-factor))) (* (double y) (Math/sin (* (double quarter-turns) pi-factor))))))
				(int (Math/round (- (* (double y) (Math/cos (* (double quarter-turns) pi-factor))) (* (double x) (Math/sin (* (double quarter-turns) pi-factor))))))
			]
)

(defn rotate [#^Integer quarter-turns src]
	(transform-coords
		(fn [[#^Integer x #^Integer y]]
			[
				(int (Math/round (+ (* x (Math/cos (* quarter-turns pi-factor))) (* y (Math/sin (* quarter-turns pi-factor))))))
				(int (Math/round (- (* y (Math/cos (* quarter-turns pi-factor))) (* x (Math/sin (* quarter-turns pi-factor))))))
			]
		)
		src
	)
) 

(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection."
  {:added "1.2"}
  [coll]
  (nth coll (rand-int (count coll))))

(defn rand-where [pred coll]
	(
		(fn [current found ss]
			(if (empty? ss)
				current
				(let [item (first ss)]
					(if (and (pred item) (== 0 (rand-int (inc found))))
						(recur item (inc found) (rest ss))
						(recur current found (rest ss))
					)
				)
			)		
		)
		nil 
		0  
		(seq coll)
	)
)

(defn mirror [src]
	(transform-coords
		(fn [[x y]]
			[
				(- x)
				y
			]
		)
		src
	)
)

; fits? is true if src doesn't contradict any cell in dst
(defn fits? [dst src]
	(subset src (paste src dst))
)

(defn crange [src]
	(reduce 
		(fn [[#^Integer minx #^Integer miny #^Integer maxx #^Integer maxy] [[#^Integer x #^Integer y] _]]
			[
				(int (Math/min minx x))
				(int (Math/min miny y))
				(int (Math/max maxx (unchecked-inc x)))
				(int (Math/max maxy (unchecked-inc y)))

			]
		) 
		[Integer/MAX_VALUE Integer/MAX_VALUE Integer/MIN_VALUE Integer/MIN_VALUE]
		src
	)
)

(defn rand-point [src]
	(nth (keys src) (rand-int (count src)))
)



(defn rand-edge-point [src]
	(rand-where
		(fn [[p s]]
			(and
				(not (= s "#"))
				(some
					#(= nil (src %))
					(neighbours p)
				)
			)
		)
		src
	)
)

(defn merge-maps [ma mb]
	(let [mn (paste ma mb)]
		(if (subset ma mn)
			mn
			nil
		)
	)
)

(defn try-times [f n]
	(if (> n 0)
		(let [res (f)]
			(if (nil? res)
				(recur f (dec n))
				res
			)
		)
		nil
	)
)

(defn read-map [#^String st]
	(let [lines (.split (.trim st) "\n")
				h (count lines)
				w (count (first lines))
				]
		(reduce
			(fn [hm [p s]] (assoc hm p s))
			{}
			(mapcat
				identity
				(for [y (range h)]
					(let [#^String line (.trim #^String (nth lines y))]
						(for [x (range w)]
							[[x y] (.substring line x (inc x))]
						)
					)
				)
			)
		)
	) 
)

(def base-maps
	[
		(read-map "
			#####
			#...#
			#....
			#...#
			##.##
		")
		(read-map "
			#.###
			#...#
			#....
			....#
			#####
		")
		(read-map "
			#.##
			#...
			####
		")
		(read-map "
			#.###.###.#
			...........
			#.###.###.#
		")
		(read-map "
			#.#
			...
			#.#
		")
		(read-map "
			###.###
			#.....#
			#.o.o.#
			.......
			#.o.o.#
			#.....#
			###.###
		")
		(read-map "
			###.###
			##...##
			#.....#
			.......
			#.....#
			##...##
			###.###
		")
		(read-map "
			###.##
			#....#
			#....#
			...... 
			#....#
			#....#
			###.##
		")
		(read-map "
			#.####
			......
			......
			#.####
		")  
		(read-map "
			#.####..#
			........#
			........#
			###.#####
		")  
		(read-map "
			#..#
			...#
			...#
			####
		")   
		(read-map "
			####
			....
			...#
			#.##
		")   

	]
) 
 
(defn map-rotations [m]
	(map
		#(rotate % m)
		'(0 1 2 3)
	)
)

(defn map-transforms [m]
	(concat
		(map-rotations m)
		(map-rotations (mirror m))
	)
)

(def maps (vec
	(mapcat
		map-transforms
		base-maps
	)
))

(defn try-grow [m1]
	(try-times
		(fn [] 
			(let [mr (rand-nth maps)
						p1 (rand-edge-point m1)
						p2 (rand-edge-point mr)
						m2 (offset mr (sub-dir (first p1) (first p2)))]
				(merge-maps m2 m1)
			)
		)
		100
	)
)

(defn on-edge8? [p m]
	(some
		(fn [d]
			(nil? (m (add-dir p d)))
		)
 		directions8
	)
)

(defn close-map [m]
	(reduce
		(fn [mp [p s]]
			(if (on-edge8? p m)
				(assoc mp p "#") 
				mp
			)
		)
		m
		m
	)
)


(defn build [n m]
	(if (> n 0)
		(recur (dec n) (or (try-grow m) m))
		m
	)
)

(defn format-map [src]
	(let [[minx miny maxx maxy] (crange src)]
		(str		
			"\n"
			(clojure.contrib.str-utils/str-join
				"\n"
				(for [ y (range miny maxy)]
					(clojure.contrib.str-utils/str-join
						""
						(for [x (range minx maxx)]
							(or (src [x y]) " ")
						)
					)
				)
			)
			"\n"
		)
	)
)

(defn print-map [src]
	(print (format-map src))
)

(def a (make-map "a" 1 1))

(def sp (make-map " " 2 2))

(def m (rand-nth maps))

(defn pm [] 
	(print-map (close-map (build 50 m)))
)

(defn gm [] 
			(let [m1 m
						mr (rand-nth maps)
						p1 (rand-edge-point m1)
						p2 (rand-edge-point mr)
						m2 (offset mr (sub-dir (first p1) (first p2)))]
				(merge-maps m2 m1)
			)
)

