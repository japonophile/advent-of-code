(ns aoc2019.d1
  (:require [clojure.test :refer [is testing with-test]])
  (:gen-class))

(defn hello []
  (println "HELLO, Advent of Code!"))

; part 1

(with-test
  (defn fuel [mass]
    (- (int (/ mass 3)) 2))
  (testing "fuel requirements"
    (is (= 2 (fuel 12)))
    (is (= 2 (fuel 14)))
    (is (= 654 (fuel 1969)))
    (is (= 33583 (fuel 100756)))))

(defn total-fuel [masses]
  (reduce + 0 (map fuel masses)))

; part 2

(with-test
  (defn full-fuel [mass]
    (loop [mass mass
           ff 0]
      (let [f (fuel mass)]
        (if (<= f 0)
          ff
          (recur f (+ ff f))))))
  (testing "fuel requirements (including fuel)"
    (is (= 2 (full-fuel 14)))
    (is (= 966 (full-fuel 1969)))
    (is (= 50346 (full-fuel 100756)))))

(defn total-full-fuel [masses]
  (reduce + 0 (map full-fuel masses)))

(defn -main []
  (hello))
