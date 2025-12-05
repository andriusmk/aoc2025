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
        find-neighbours (fn [m pos]
                          (let [[px py] (map2d/pos-xy m pos)]
                            (for [dx (range -1 2)
                                  dy (range -1 2)
                                  :when (not (= dx dy 0))]
                              (map2d/get m [(+ px dx) (+ py dy)] \.))))
        valid-pos (fn [m pos]
                    (let [neighbours (find-neighbours m pos)]
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
  (time (solution (aoc/read-input input-file)))
  ;;
  )
