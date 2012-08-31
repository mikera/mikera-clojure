(ns test.foo)

(defprotocol InternalReduce
  "Protocol for concrete types that can reduce themselves
   faster than first/next recursion. Called by clojure.core/reduce."
  (internal-reduce 
    [seq f] 
    [seq f start]))

(extend-protocol InternalReduce
  nil
  (internal-reduce
    ([s f] 
      (f))
    ([s f val]
      val))
  
  ;; handle reducible collection types directly
  clojure.lang.IReduce
  (internal-reduce
    ([s f]
      (.reduce s f))
    ([s f val]
      (.reduce s f val)))
  
  ;; special case handling for strings  
  java.lang.String
  (internal-reduce
    ([s f]
      (if (= 0 (.length s))
        (f)
	        (loop [i 1
                 val ^Object (.charAt s i)]
		        (if (< i (.length s))
		          (recur (inc i) (f val (.charAt s i)))
		          val))))
    ([s f val]
      (loop [i 0
             val val]
        (if (< i (.length s))
          (recur (inc i) (f val (.charAt s i)))
          val))))
  
  clojure.lang.ISeq
  (internal-reduce
    ([s f]
      (internal-reduce (next s) f (first s)))
    ([s f val]
	   (loop [cls (class s)
	          s (seq s)
	          f f
	          val val]
	     (if s
	       ;; roll over to faster implementation if underlying seq changes type
	       (if (identical? (class s) cls)
	         (recur cls (next s) f (f val (first s)))
	         (internal-reduce s f val))
	       val))))

   java.lang.Object
  (internal-reduce
    ([s f]
      (if-let [ss ^clojure.lang.ISeq (seq s)]
        (internal-reduce (next ss) f (first ss))
        (f)))
    ([s f val]
      (if-let [ss ^clojure.lang.ISeq (seq s)]      
        (internal-reduce ss f val)
        val)))
  
  )
 
(def arr-impl
  '(internal-reduce
     ([a-seq f]
       (let [arr (.array a-seq)
             end (alength arr)
             i (.index a-seq)]
         (if (< i end)
           (loop [offset (inc i)
                  val ^Object (aget arr i)]
             (if (< offset end)
               (recur (inc offset) (f val (aget arr offset)))
               val))
           (f))))
     ([a-seq f val]
       (let [arr (.array a-seq)]
         (loop [i (.index a-seq)
                val val]
           (if (< i (alength arr))
             (recur (inc i) (f val (aget arr i)))
             val))))))

(defn- emit-array-impls*
  [syms]
  (apply
   concat
   (map
    (fn [s]
      [(symbol (str "clojure.lang.ArraySeq$ArraySeq_" s))
       arr-impl])
    syms)))

(defmacro emit-array-impls
  [& syms]
  `(extend-protocol InternalReduce
     ~@(emit-array-impls* syms)))

(emit-array-impls int long float double byte char boolean)
