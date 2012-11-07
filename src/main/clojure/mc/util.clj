(ns mc.util
  (:import mikera.util.Rand)
  (:import [mikera.util Tools])
  (:import [mikera.clojure ClojureError])
  (:import [clojure.lang IDeref])
  (:require [clojure.set])
  (:use [clojure.test]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro error
  "Throws an error with the provided message(s)"
  ([& vals]
    `(throw (mikera.clojure.ClojureError. (str ~@vals)))))

(defn debug [object]
  (mikera.util.Tools/debugBreak object))

(defn array-concat [a b]
  (let [la (count a)
        lb (count b)
        new-length (+ la lb)
        target (double-array new-length)]
    (System/arraycopy a 0 target 0 la)
    (System/arraycopy b 0 target la lb)
    target))

(defn run-at-intervals [f interval-ms ms]
  "Sets off a thread with a function that will be run at intervals for the given time period"
  (future
	  (let [start-time (System/currentTimeMillis)
	        end-time (+ start-time ms)
	        loop-count (atom 0)]
	    (while (< (System/currentTimeMillis) end-time) (do
	      (Thread/sleep interval-ms)
	      (f))))))

(defn run-loop [f ms]
  "Runs a function repeatedly in a loop for a given number of millisenonds"
  (let [start-time (System/currentTimeMillis)
        end-time (+ start-time ms)
        loop-count (atom 0)]
    (while (< (System/currentTimeMillis) end-time) (do
      (f)
      (swap! loop-count inc)))
    {:time ms 
     :iterations @loop-count 
     :rate (double (/ (* @loop-count 1000) ms))}))

(defn find-first [pred coll]
  "Searches a collection and returns the first item for which pred is true, nil if not found"
  (loop [s (seq coll)] 
    (when s  
      (let [v (first s)]
        (if (pred v)
          v
          (recur (next s)))))))

(defn find-position 
  "Searches a collection and returns the index of the item's position"
  ([coll item] 
    (find-position coll item 0))
  ([coll item i] 
    (if (empty? coll) 
      nil
	    (let [v (first coll)]
	      (if (= item v)
	        i
	        (recur (rest coll) item (inc i)))))))

(defn middle [x y z]
  "Returns the middle value of 3 numbers, which may be in any order" 
  (if (> x y)
    (if (<= x z) 
      x
      (max y z))
    (if (<= y z) 
      y
      (max x z))))


(defn reduce-indexed 
  "Reduce while adding an index as the second argument to the function"
  ([f coll]
    (reduce-indexed f (first coll) 0 (rest coll)))

  ([f init coll]
    (reduce-indexed f init 0 coll))

  ([f init ^long i coll]
    (if (empty? coll)
      init
      (let [v (first coll)
            fv (f init i v)]
        (recur f fv (inc i) (rest coll))))))

(defn map-vector 
  "Applies a function f to every value of a vector v as in map, non-lazy"
  ([f v]
    (map-vector f v []))
  ([f v result]
    (if (empty? v)
      result
      (recur f (rest v) (conj result (f (first v)))))))

(defn remove-nth
  "Removes the item at position n from a vector, returning a shrunk vector"
  ([avector n]
    (vec
      (concat
        (subvec avector 0 n) (subvec avector (inc n) (count avector))))))

  
(defn remove-nils 
  "Removes all nils for a sequence" 
  ([[x & xs]]
	  (let [rest (if (empty? xs) '() (remove-nils xs))]
	    (if (nil? x)
	      rest
	      (cons x rest)))))


(defn list-not-nil [& xs]
  (remove-nils xs))

(defmacro applyn 
  "Applies a function n times (nested) to an argument"
  ([f n x] 
    (reduce (fn [a _] (list f a)) x (range n))))

(defmacro repeated-apply [f n] 
  (cond 
    (<= n 0) identity
    true `(let [f# ~f] (fn [a#] ((repeated-apply f# ~(dec n)) a#)))))

(defn get-element-after [coll value]
  (if (= value (first coll))
    (first (rest coll))
    (recur (rest coll) value)))

(defn list-contains? 
  "Returns true if a collection contains a specific value, false otherwise"
  ([coll value]
    (if-let [s (seq coll)]
      (if (= (first s) value) true (recur (rest s) value))
      false)))

(defn map-difference2 [m1 m2]
  "Gets the difference between two maps"
  (loop [m (transient {})
         ks (concat (keys m1) (keys m2))]
    (if-let [k (first ks)]
      (let [e1 (get m1 k)
            e2 (get m2 k)]
        (cond 
          (= e1 e2) 
            (recur m (rest ks))
          (not e1) 
            (recur (assoc! m k nil) (rest ks))
          :else    
            (recur (assoc! m k e1) (rest ks))))
      (persistent! m))))

(defn map-difference3 [m1 m2]
  "Gets the difference between two maps"
  (if (nil? m2)
    m1
	  (let [md (mikera.util.Tools/mapDifference m1 m2)]
	    (merge {} md))))

(defn map-difference [m1 m2]
  "Gets the difference between two maps"
  (loop [m {}
         ks (concat (keys m1) (keys m2))]
    (if-let [k (first ks)]
      (let [v1 (get m1 k)
            v2 (get m2 k)]
        (cond 
          (= v1 v2) 
            (recur m (rest ks))
          (not v1) 
            (recur (assoc m k nil) (rest ks))
          :else    
            (recur (assoc m k v1) (rest ks))))
      m)))

; (time (dotimes [i 1000] (map-difference (:game @state) nil)))

(defn rand-choice 
  "Returns a randomly selected item from a collection"
  ([coll]
    (nth coll (Rand/r (count coll)))))

(defn argmax 
  "Returns the item than maximises a given function" 
  ([f items]
    (let [v (first items)] 
      (argmax f (rest items) (f v) v )))
  ([f items bestvalue best]
    (if-let [items (seq items)] 
      (let [v (first items)
            fv (f v)] 
	      (if (> fv bestvalue)
	        (recur f (rest items) fv v)
	        (recur f (rest items) bestvalue best)))
      best)))

(defn valmax 
  "Gets the maximum value of a function over a given set of items" 
  ([f items]
    (let [v (first items)] 
      (valmax f (rest items) (f v) v )))
  ([f items bestvalue best]
    (if-let [items (seq items)] 
      (let [v (first items)
            fv (f v)] 
        (if (> fv bestvalue)
          (recur f (rest items) fv v)
          (recur f (rest items) bestvalue best)))
      bestvalue)))

(defn round 
  "Rounds a fractional value" 
  ([x]
    (Math/round (double x)))
  ([x decimal-places]
    (throw (Error. "Not implemented yet!"))))

;; threadlocal macros from amalloy's flatland/useful library



(defn ^{:dont-test "Used in impl of thread-local"}
  thread-local*
  "Non-macro version of thread-local - see documentation for same."
  [init]
  (let [generator (proxy [ThreadLocal] []
                    (initialValue [] (init)))]
    (reify IDeref
      (deref [this]
        (.get generator)))))

(defmacro thread-local
  "Takes a body of expressions, and returns a java.lang.ThreadLocal object.
   (see http://download.oracle.com/javase/6/docs/api/java/lang/ThreadLocal.html).

   To get the current value of the thread-local binding, you must deref (@) the
   thread-local object. The body of expressions will be executed once per thread
   and future derefs will be cached.

   Note that while nothing is preventing you from passing these objects around
   to other threads (once you deref the thread-local, the resulting object knows
   nothing about threads), you will of course lose some of the benefit of having
   thread-local objects."
  [& body]
  `(thread-local* (fn [] ~@body)))

(defn split-equally [num coll] 
  "Split a collection into a vector of (as close as possible) equally sized parts"
  (loop [num (long num) 
         parts []
         coll coll
         c (count coll)]
    (if (<= num 0)
      parts
      (let [t (quot (+ c num -1) num)]
        (recur (dec num) (conj parts (take t coll)) (drop t coll) (- c t)))))) 

(defmacro xor 
  ([] nil)
  ([a] a)
  ([a & more]
    `(let [a# ~a
           b# (xor ~@more)]
       (if a#
         (if b# nil a#)
         b#))))

(defmacro for-loop 
  "Runs an imperatibve for loop, binding sym to init, running code as long as check it true, updating sym according to change"
  ([[sym init check change :as params] & code]
	  (cond
	    (not (vector? params)) 
	      (throw (Error. "Binding form must be a vector for for-loop"))
	    (not= 4 (count params)) 
	      (throw (Error. "Binding form must have exactly 4 arguments in for-loop"))
	    :default
	      `(loop [~sym ~init value# nil]
	         (if ~check
	           (let [new-value# (do ~@code)]
	             (recur ~change new-value#))
	           value#)))))

(defmacro do-indexed 
  "loops over a set of values, dinding index-sym to the 0-based index of each value"
  ([[val-sym values index-sym initial] & code]
  `(if-let [vals# (seq ~values)]
     (loop [vals# vals#
            i# ~initial]
       (let [~val-sym (first vals#)
             ~index-sym i#]
         ~code)
       (if-let [next# (next vals#)] 
         (recur next# (inc i#))
         nil)))))

(comment
  (for-loop [i 0 (< i 10) (inc i)]
     (println i))
)
