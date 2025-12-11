(ns aoc.aoc2025.day10-1
  (:require [aoc.core :as aoc]
            [clojure.string :as string]))

(def test-input "Example from AOC" "
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
")

(def input-file "data/input-10.txt")

(defn parse-lights [field]
  (let [pattern (subs field 1 (dec (count field)))]
    (->> pattern
         (map (fn [idx c]
                (if (= c \#)
                  (bit-shift-left 1 idx)
                  0))
              (range))
         (apply bit-or))))

(defn parse-button [field]
  (as-> field <>
    (subs <> 1 (dec (count field)))
    (string/split <> #",")
    (map #(bit-shift-left 1 (parse-long %)) <>)
    (apply (partial bit-or 0) <>)
    ))

(defn parse-line [line]
  (let [fields (string/split line #"\s+")
        light-field (fields 0)
        buttons (drop-last (rest fields))]
    {:lights (parse-lights light-field)
     :buttons (mapv parse-button buttons)}))

(defn parse-input [input]
  (map parse-line input))

(defn solution [input]
  (parse-input input))

(comment
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
