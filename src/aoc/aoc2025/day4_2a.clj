(ns aoc.aoc2025.day4-2a
  (:require [aoc.core :as aoc]
            [aoc.map2d :as map2d]
            [clojure.string :as string]))

(def test-input "
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
")

(def input-file "data/input-4.txt")

(defn solution [input]
  (let [initial-map (map2d/create-map input)
        neighb-off (for [x (range -1 2)
                         y (range -1 2)
                         :when (not (= x y 0))]
                     [x y])
        valid-pos (fn [m pos]
                    (let [neighbours (map (comp #(map2d/get m % \.)
                                                (partial mapv + (map2d/pos-xy m pos)))
                                          neighb-off)]
                      (< (count (filter (partial = \@)
                                        neighbours))
                         4)))]
    (loop [ans 0
           curr-map initial-map]
      (let [valid-poss (filter (fn [[idx c]] (and (= c \@) (valid-pos curr-map idx)))
                               (map2d/enumerate curr-map))
            new-map (apply (partial map2d/set-to curr-map \.)
                           (map first valid-poss))]
        (if (empty? valid-poss) ans
            (recur (+ ans (count valid-poss)) new-map))))))

(comment
  (solution (aoc/parse-test test-input)) ; must be 43
  (solution (aoc/read-input input-file))
  (map (partial apply str) (split-at 2 "abcd"))
  ;;
  )
