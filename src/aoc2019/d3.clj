(ns aoc2019.d3
  (:require
    [clojure.string :as s]
    [clojure.test :refer [is testing with-test]])
  (:gen-class))

; part 1

(with-test
  (defn axis-offset
    "determine axis (:v or :h) and offset from direction and distance"
    [dir dist]
    (condp = dir
      \U [:v [0 dist]]
      \D [:v [0 (- dist)]]
      \R [:h [dist 0]]
      \L [:h [(- dist) 0]]))
  (defn apply-move
    "apply a move m to the current position, and returns the next position and wire segments"
    [cp ws m]
    (let [dir (first m)
          dist (Integer/parseInt (s/join (rest m)))
          [ax of] (axis-offset dir dist)
          np (vec (map + cp of))]
      [np (update-in ws [ax] conj [cp np])]))
  (defn wire-segments
    "compute the wire segments for a sequence of moves"
    [cp wire]
    (loop [cp cp              ; current position
           ws {:v [], :h []}  ; wire segments (result)
           ms (s/split wire #",")]  ; moves
      (if-let [m (first ms)]
        (let [[np nws] (apply-move cp ws m)]
          (recur np nws (rest ms)))
        ws)))
  (defn intersect [h v]
    (let [[[x1 y] [x2 _]] h
          [[x y1] [_ y2]] v]
      (when (and (or (<= x1 x x2) (<= x2 x x1))
                 (or (<= y1 y y2) (<= y2 y y1)))
        [x y])))
  (defn intersect-hor-ver [hs vs]
    (filter some?
            (for [h hs
                  v vs]
              (intersect h v))))
  (defn intersections [ws1 ws2]
    (vec (concat
           (intersect-hor-ver (:h ws1) (:v ws2))
           (intersect-hor-ver (:h ws2) (:v ws1)))))
  (defn closest-distance [cp ps]
    (let [[x0 y0] cp
          ds (map #(+ (Math/abs (- (first %) x0))
                      (Math/abs (- (second %) y0))) ps)
          sorted-ds (filter #(> % 0) (sort ds))]
      (first sorted-ds)))
  (defn distance-intersect-to-pole [wire1 wire2]
    (let [cp  [0 0]
          ws1 (wire-segments cp wire1)
          ws2 (wire-segments cp wire2)
          ps  (intersections ws1 ws2)
          d   (closest-distance cp ps)]
      d))
  (testing "manhattan distance from central pole to closest intersection"
    (is (= 159
           (distance-intersect-to-pole
             "R75,D30,R83,U83,L12,D49,R71,U7,L72"
             "U62,R66,U55,R34,D71,R55,D58,R83")))
    (is (= 135
           (distance-intersect-to-pole
             "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
             "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))
