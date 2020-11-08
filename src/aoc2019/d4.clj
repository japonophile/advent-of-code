(ns aoc2019.d4
  (:require
    [clojure.string :as s]
    [clojure.test :refer [is testing with-test]])
  (:gen-class))

; part 1

(with-test
  (defn meets-criteria?
    [n]
    (let [d (str n)]
      (and
        (or (= (get d 0) (get d 1))
            (= (get d 1) (get d 2))
            (= (get d 2) (get d 3))
            (= (get d 3) (get d 4))
            (= (get d 4) (get d 5)))
        (<= (int (get d 0))
            (int (get d 1))
            (int (get d 2))
            (int (get d 3))
            (int (get d 4))
            (int (get d 5))))))
  (testing "meets password criteria"
    (is (= true (meets-criteria? 111111)))
    (is (= false (meets-criteria? 223450)))
    (is (= false (meets-criteria? 123789)))))

(defn count-passwords
  [rng]
  (let [[st en] (s/split rng #"-")
        start (Integer/parseInt st)
        end   (Integer/parseInt en)]
    (println start end)
    (count (filter true? (map meets-criteria? (range start (inc end)))))))
