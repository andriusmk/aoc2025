(ns aoc.aoc2025.day9-2
  (:require [aoc.core :as aoc]
            [clojure.string :as string]))

(def test-input "Example from AOC" "
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
")

(def input-file "data/input-9.txt")

(defn parse-input [input]
  (mapv (comp (partial mapv parse-long) #(string/split % #",")) input))

(defn area [v1 v2]
  (apply * (map (comp inc abs -) v1 v2)))

(defn flip-v [[x1 y1] [x2 y2]]
  [[x1 y2] [x2 y1]])

(defn flip-h [[x1 y1] [x2 y2]]
  [[x2 y1] [x1 y2]])

(defn pos-rec [vectors t1 t2]
  (let [[[x1 y1 :as v1] [x2 y2 :as v2]] (mapv vectors [t1 t2])]
    (cond->> [v1 v2]
      (< x1 x2) (apply flip-v)
      (< y1 y2) (apply flip-h))))

(defn inside? [vectors [t1 t2] t]
  (let [[[x1 x2] [y1 y2]] (pos-rec vectors t1 t2)
        [x y] t]
    (and (> x1 x x2)
         (> y1 y y2))))

(defn make-pairs [size]
  (for [x (range 0 (dec size))
        y (range (inc x) size)]
    [x y]))

(defn calc-areas [vectors]
  (map #(vector % (apply area (map vectors %))) (make-pairs (count vectors))))

(defn solution [input]
  (let [vectors (parse-input input)]
    (->> vectors
         calc-areas
         (apply max-key second)
         second)))

(comment
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
