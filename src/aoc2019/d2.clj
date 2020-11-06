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
