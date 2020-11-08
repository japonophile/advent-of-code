(ns aoc2019.d2
  (:require [clojure.test :refer [is testing with-test]])
  (:gen-class))

; part 1

(with-test
  (defn exec [op intcode ip]
    (assoc intcode (get intcode (+ 3 ip))
           (op (get intcode (get intcode (inc ip)))
               (get intcode (get intcode (+ 2 ip))))))
  (defn run
    ([intcode ip]
     (condp = (get intcode ip)
       99 intcode
       1  (recur (exec + intcode ip) (+ 4 ip))
       2  (recur (exec * intcode ip) (+ 4 ip))))
    ([intcode]
     (run intcode 0)))
  (testing "running intcode"
    (is (= [3500 9 10 70 2 3 11 0 99 30 40 50]
           (run [1 9 10 3 2 3 11 0 99 30 40 50])))
    (is (= [2 0 0 0 99]
           (run [1 0 0 0 99])))
    (is (= [2 3 0 6 99]
           (run [2 3 0 3 99])))
    (is (= [2 4 4 5 99 9801]
           (run [2 4 4 5 99 0])))
    (is (= [30 1 1 4 2 5 6 0 99]
           (run [1 1 1 4 99 5 6 0 99])))))

; part 2

(defn init-program [n v]
  (-> [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,6,19,23,2,6,23,27,1,5,27,31,2,31,9,35,1,35,5,39,1,39,5,43,1,43,10,47,2,6,47,51,1,51,5,55,2,55,6,59,1,5,59,63,2,63,6,67,1,5,67,71,1,71,6,75,2,75,10,79,1,79,5,83,2,83,6,87,1,87,5,91,2,9,91,95,1,95,6,99,2,9,99,103,2,9,103,107,1,5,107,111,1,111,5,115,1,115,13,119,1,13,119,123,2,6,123,127,1,5,127,131,1,9,131,135,1,135,9,139,2,139,6,143,1,143,5,147,2,147,6,151,1,5,151,155,2,6,155,159,1,159,2,163,1,9,163,0,99,2,0,14,0]
      (assoc 1 n)
      (assoc 2 v)))

(defn find-noun-verb-to-produce [expected-output]
  (loop [n 0
         v 0]
    (if (= expected-output (first (run (init-program n v))))
      (+ (* 100 n) v)
      (if (< n 99)
        (recur (inc n) v)
        (if (< v 99)
          (recur 0 (inc v))
          nil  ; not found
          )))))
