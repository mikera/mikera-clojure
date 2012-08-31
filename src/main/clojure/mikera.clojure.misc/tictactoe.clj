(ns test.tictactoe)

(def rules
  {:initial-state "........."
   :moves (fn [player state]
            (filter #(not nil? %)
              (for [i (range 9)]
                (if (= '.' (state i))
                  (str (subs state 0 i) player (subs state (inc i)))
                  nil))))
   :result nil})
          
    
   
(defn minimax [state]
  nil)