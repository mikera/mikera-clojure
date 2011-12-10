(ns test.dispatcher)

(def pred-list (ref []))

(defn dispatch-function [& args] 
  (loop [i 0]
    (cond
      (>= i (count @pred-list))     (throw (Error. "No matching function!"))
      (apply (@pred-list i) args)         i
      :else                         (recur (inc i)))))

(defmulti handler dispatch-function)
  
(defn assign-operation [function & preds]    
  (dosync
    (let [i (count @pred-list)]
      (alter pred-list conj 
             (fn [& args] (every? identity (map #(%1 %2) preds args))))
      (defmethod handler i [& args] (apply function args)))))  

(assign-operation (fn [x] (/ x 2)) even?)

(assign-operation (fn [x] (+ x 1)) odd?)

(take 15 (iterate handler 77))
