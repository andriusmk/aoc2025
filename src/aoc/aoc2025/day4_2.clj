(ns aoc.aoc2025.day4-2
  (:require [aoc.core :refer [parse-test read-input]]
            [clojure.set :as set]))

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

(defn get-occupied [input]
  (set (for [x (range (count (input 0)))
             y (range (count input))
             :when (= \@ (nth (input y) x))]
         [x y])))

(defn solution [input]
  (let [neighb-off (list [-1 -1] [-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1])
        neighbours (fn [pos] (set (map #(mapv + pos %) neighb-off)))
        neighb-cnt (fn [occupied pos] (count (set/intersection (neighbours pos) occupied)))
        valid? (fn [occupied pos] (< (neighb-cnt occupied pos) 4))
        find-valid (fn [occupied] (set (filter (partial valid? occupied) occupied)))
        rec-f (fn [occupied ans]
                (let [removable (find-valid occupied)]
                  (if (empty? removable) ans
                      (recur (set/difference occupied (set removable)) (+ ans (count removable))))))]
    (rec-f (get-occupied input) 0)
    ;
    ))

(comment
  (solution (parse-test test-input)) ; must be 43
  (solution (read-input input-file))
  ;;
  )
