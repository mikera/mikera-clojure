(ns utiltest
  (:use clojure.test)
  (:use mc.util))

(deftest test-applying
  (testing "applyn"
    (is (= 810 (applyn inc 10 800)))
    (is (= `(inc (inc 6)) (macroexpand `(applyn inc 2 6))))))


(deftest test-array-concat 
  (let [a1 (double-array [0.1 0.2])
        a2 (double-array [0.3 0.4])
        a (array-concat a1 a2)]
    (is (= 0.3 (aget a 2)))
    (is (= 0.2 (aget a 1)))
    (is (= 4 (count a)))))

(deftest test-for-loop
  (is (= "A" (for-loop [i 0 (< i 10) (inc i)] "A")))
  (is (= nil (for-loop [i 0 (< i 0)  (inc i)] "A")))
  (is (= 4 (for-loop [i 0 (< i 1)  (inc i)] 1 2 3 4)))
  (is (= 10 (let [a (atom 0)]
              (for-loop [i 0 (< i 10) (inc i)] (swap! a inc))
              @a))))

(deftest test-middle
  (is (= 2 (middle 1 2 3)))
  (is (= 2 (middle 3 1 2)))
  (is (= 2 (middle 2 3 1)))
  (is (= 2 (middle 2 2 2)))
  (is (= 2 (middle 2 1 2))))

(deftest test-map-difference
  (is (= {} (map-difference {:a 1 :b 2} {:a 1 :b 2})))
  (is (= {:a 1} (map-difference {:a 1 :b 2} {:b 2})))
  (is (= {:a 1 :b 2} (map-difference {:a 1 :b 2} {})))
  (is (= {:a 1 :b nil} (map-difference {:a 1} {:b 2}))))

(deftest test-list-not-nil
  (is (= '() (list-not-nil nil nil)))
  (is (= '() (list-not-nil nil)))
  (is (= '(1 2 3 4 5) (list-not-nil nil 1 nil 2 3 4 5))))

(deftest test-find-position
  (is (= nil (find-position nil "fvfvf")))
  (is (= 2 (find-position [1 2 3 4] 3)))
  (is (= nil (find-position [1 2 3 4] 5))))


(deftest test-list-contains
  (is (list-contains? [1 2 3 4 5] 4))
  (is (list-contains? '(1 2 3 4 5) 4))
  (is (not (list-contains? '(1 2 3 4 5) 0)))
  (is (not (list-contains? nil 0))))

(deftest test-argmax
  (let [f (fn [x] (* x x))]
    (is (= 2 (argmax inc [2])))
    (is (= -10 (argmax f [-10 4 6])))))

(deftest test-valmax
  (let [f (fn [x] (* x x))]
    (is (= 3 (valmax inc [2])))
    (is (= 7 (valmax inc [-10 4 6])))))
