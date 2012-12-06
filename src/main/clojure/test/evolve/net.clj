(ns test.evolve.net
  (:use [clojure.tools.macro]))

(set! *warn-on-reflection* true)

(defprotocol AIContext  
  (array-input [c] "Gets the input array")
  (array-output [c] "Gets the output array")
  (array-temp [c] "Gets the output array")
  (offset-input [c] "Gets the input array offset")
  (offset-output [c] "Gets the output array offset")
  (offset-temp [c] "Gets the output array offset"))


(defprotocol Thinker  
  (run [t #^AIContext context] "Runs the thinker on a given array input")
  (size-input [t] "Gets the input size")
  (size-output [t]"Gets the output size")
  (size-temp [t]"Gets the minimum amount of temporary space required")
  (size-temp [t] "Gets the minimum amount of temporary space required"))


(defrecord NeuralNet [nodes] 
  Thinker
  (run [t context] 
    (for [node nodes]
      (run node context))))
 
(defmacro f-sigmoid [x] 
   (let [x1 (gensym "x")]
    `(let [~x1 ~x] 
		  (if 
		    (< ~x1 -88) 
        (float 0)
		    (let [~x1 (float (. java.lang.Math exp (- ~x1)))]
		      (/ 1 (+ 1 ~x1)))))))

(defmacro f-identity [x] 
   (let [x1 (gensym "x")]
    `(let [~x1 ~x] 
       ~x1)))

(defmacro f-hump [x] 
  (let [x1 (gensym "x")]
    `(let [~x1 ~x] 
        (/ (+ (float 1) (* ~x1 ~x1))))))

(defmacro f-step [x] 
  (let [x1 (gensym "x")]
    `(let [~x1 ~x] 
        (if (> ~x1 0) (float 1) (float 0)))))

(def functions [`f-sigmoid `f-identity `f-hump `f-step])

 
(defmacro testsum [func n] 
  (let [x (gensym "x")]
    `(fn [~x] (+ ~@( map (fn [i] `(~func ~x)) (range n))))))

(defmacro compile-node-input-sum [data weights]
    `(+ 
        ~@(map 
          (fn [[i w]] 
            (if 
              (= :constant i)
              `(float ~w) 
              `(* (aget ~data ~i) (float ~w)))) 
          weights)))
     
(defmacro compile-node-function [func weights]
  (let [data (with-meta (gensym "data") {:tag 'floats})
        inputsum `(compile-node-input-sum ~data ~weights)]
  `(fn [~data] 
     (~func ~inputsum))))

; (mexpand `(compile-node-input-sum data [[0 0.5] [1 2.3]]) )
; (mexpand-all `(compile-node-function f-sigmoid [[0 0.5] [1 2.3]]) )

(defrecord Node [function weights])

(defn make-positions [inputlength nodes]
  (let [nodepos (atom {})
        nextpos (atom inputlength)]
    (doseq [n nodes]
      (do 
        (swap! nodepos assoc n @nextpos)
        (swap! nextpos inc)))
    @nodepos))
   
; (make-positions 10 [:a :b :c])

(def n1 (Node. `f-sigmoid [[1 0.5] [:constant 2.1]]))

(defmacro get-weights [node]
  `(str ~( :weights node)))

(defn make-weights [node nodepositions]
  (vec (map 
     (fn [[srcnode weight]]
       [(or (nodepositions srcnode) srcnode) weight]) ; if node is an int, keep it   
     (:weights node))))

; (make-weights n1 (make-positions 10 [n1])) 

(defmacro compile-node [data node nodepositions] 
   `(aset 
      ~data 
      ~(nodepositions node) 
      (float 
        ((compile-node-function 
          ~(:function node) 
          ~(make-weights node nodepositions)) 
          ~data))))
  
; (let [n (Node. f-sigmoid [[1 0.5]])] (make-weights n (calc-positions 10 [n])))

(defmacro compile-nodes [inputlength nodes]
  (let [enodes (eval nodes)
        nodepos (make-positions inputlength enodes)
        data (with-meta (gensym "data") {:tag 'floats})]
    `(fn [~data] 
      (do 
        ~@(map
            (fn [node] `(compile-node ~data ~node ~nodepos))
            enodes)
        ~data))))

(defn order-nodes1 [posmap nodesout nodesin]
  (let [node (first nodesin)]
    (cond
      (nil? node) nodesout
      (posmap node) (recur posmap nodesout (rest nodesin)))))

(defmacro order-nodes [nodes]
  (order-nodes1 {} [] nodes))

#_(let [t (compile-nodes 10 [n1 n1 n1 n1 n1 n1 n1 n1 n1 n1])
      arr (make-array Float/TYPE 20)] 
  (dotimes [_ 5] (time (dotimes [_ 1000] (t arr))))) 

(defmacro testfn [node] 
  `( ~(:function node) ~(( (:weights node ) 0 ) 0)))
   
; (mexpand-all `(compile-nodes 10 [n1])) 

; (vec ((compile-nodes 10 [n1]) (make-array Float/TYPE 20)))
