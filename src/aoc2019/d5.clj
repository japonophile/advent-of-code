(ns aoc2019.d5
  (:require [clojure.test :refer [is testing with-test]])
  (:gen-class))

; part 1

(with-test
  (defn get-val [intcode p m]
    (if (= 1 m)
      (get intcode p)
      (get intcode (get intcode p))))
  (defn assocx [vect i v]
    (if (< i (count vect))
      (assoc vect i v)
      (do
        (println (str "extend v to " (inc i)))
        (assoc (reduce (fn [vv _] (conj vv 0)) vect (range (count vect) (inc i))) i v))))
  (defn exec [op intcode ip C B A]
    (println (str (get-val intcode (inc ip) C) " " op " "
                  (get-val intcode (+ 2 ip) B) " -> "
                  (get-val intcode (+ 3 ip) A)))
    (assocx intcode (get-val intcode (+ 3 ip) A)
            (op (get-val intcode (inc ip) C)
                (get-val intcode (+ 2 ip) B))))
  (defn save [intcode ip v C]
    (println (str v " => " (get-val intcode (inc ip) C)))
    (assocx intcode (get-val intcode (inc ip) C) v))
  (defn run
    ([intcode ip input]
     (let [ir (get intcode ip)
           A  (quot ir 10000)
           B  (mod (quot ir 1000) 10)
           C  (mod (quot ir 100) 10)
           oc (mod ir 100)]
     (condp = oc
       99 intcode
       1  (recur (exec + intcode ip C B A) (+ 4 ip) input)
       2  (recur (exec * intcode ip C B A) (+ 4 ip) input)
       3  (recur (save intcode ip (first input) C) (+ 2 ip) (rest input))
       4  (do (println (get-val intcode (inc ip) C))
              (recur intcode (+ 2 ip) input)))))
    ([intcode input]
     (run intcode 0 input)))
  (testing "running intcode"
    ))
