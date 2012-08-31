(ns test.monadtest
  (:use [clojure.contrib.monads])
  (:use [clojure.contrib.macro-utils]))

(defmonad PairState
  [m-result (fn [v] (fn [s1 s2] [v s1 s2]))
   m-bind (fn [a f]
            (fn [s1 s2] 
              (let [nmv (a s1 s2)]
                (if (nil? nmv)
                  nil
                  (let [[nv ns1 ns2] nmv]
                    ((f nv) ns1 ns2))))))
   m-zero (fn [s1 s2] nil)])

(def mfirst
  (fn [s1 s2]
    [s1 s1 s2]))

(def ma 
  (domonad PairState
    [a (m-result 4)
     b (m-result 7)
     c mfirst] 
    (+ a b c)))


(defprotocol PStateStore 
  (set-store-state [ss v])
  (get-store-state [ss])
  (set-store-value [ss v])
  (get-store-value [ss]))

(deftype StateStore [^{:unsynchronized-mutable true} value 
                     ^{:unsynchronized-mutable true} state]
  PStateStore 
	  (get-store-state [ss] (.state ss))
	  (get-store-value [ss] (.value ss))
	  (set-store-state [ss v] (set! state v))
	  (set-store-value [ss v] (set! value v))
   
   Object
     (toString [ss] (str "value=" (.value ss) ", state=" (.state ss))))

(defn state-store [v s] (StateStore. v s))

(defmonad MStoredState
  [m-result (fn [v] 
              (fn [^StateStore ss] 
	              (do
	                (set-store-value ss v)
	                ss)))
   m-bind (fn [a f]
            (fn [^StateStore ss]
              (do
                (a ss)
                ((f (get-store-value ss)) ss))))])

(def mb
  (domonad MStoredState
    [a (m-result 1)
     b (m-result 5)]
    (+ a b)))

(def ssa (state-store 100 101))