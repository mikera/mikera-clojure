(ns mikera.clojure.ai.bayes
  "A simple naive bayes classifier in Clojure")

(def empty-model "A model with no data"
  {:frequencies-by-class nil
   :features-given-class nil
   :features nil})

(defn inc-map [m v] "Increments a value in a map with 1 as a default value"
  (assoc m v (inc (get m v 1))))

(defn prob [m v] "Gets a probability of a value within a map of frequencies"
  (/ (double (m v)) (reduce + (vals m))))

(defn learn [model [[& fs] c]]
  (let [{fbc :frequencies-by-class fgc :features-given-class fe :features} model]
    {:frequencies-by-class (inc-map fbc c)
     :features-given-class (update-in fgc [c] #(mapv inc-map (or % (mapv (constantly nil) fs)) fs))
     :features (mapv inc-map (or fe (mapv (constantly nil) fs)) fs)})) 

(defn class-probability [model [& features] c]
  (let [{fbc :frequencies-by-class fgc :features-given-class fe :features} model]
    (* (prob fbc c)
       (apply * (map prob (fgc c) features))
       (/ 1.0 (apply * (map prob fe features))))))

