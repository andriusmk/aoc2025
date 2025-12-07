(ns aoc.aoc2025.day7-1
  (:require [aoc.core :as aoc]
            [clojure.string :as string]
            [aoc.map2d-2 :as map2d2]))

(def test-input "Example from AOC" "
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(def input-file "data/input-7.txt")

(defn parse-input [input]
  (let [field (map2d2/create-map input)
        initial-status {:splitters #{} :box (map2d2/get-box field)}]
    (loop [status initial-status
           [[pos cell] & remaining :as cells] (map2d2/enumerate field)]
      (if (empty? cells) status
          (recur (case cell
                   \^ (update status :splitters #(conj % pos))
                   \S (assoc status :start pos)
                   status)
                 remaining)))))

(defn solution [input]
  (let [leftright (list [-1 0] [1 0])
        down [0 1]
        {:keys [box splitters start]} (parse-input input)
        initial-status {:beams (list start)
                        :score 0}
        propagate (fn [beam]
                    (let [new-pos (map2d2/rel-pos box beam down)]
                      (if (splitters new-pos)
                        (map (partial map2d2/rel-pos box new-pos) leftright)
                        (list new-pos))))]
    (loop [status initial-status]
      (if (not (reduce #(and %1 %2) true (status :beams)))
        (status :score)
        (let [new-beam-groups (map propagate (status :beams))
              new-score (+ (status :score) (apply + (map (comp dec count) new-beam-groups)))
              new-beams (distinct (apply concat new-beam-groups))] 
          (recur {:beams new-beams :score new-score}))))
    ))

(comment
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
