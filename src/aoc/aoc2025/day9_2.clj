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
