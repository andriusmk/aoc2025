(ns aoc.aoc2025.day7-2
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
  (let [{:keys [box splitters start]} (parse-input input)
        [initial-beam initial-y] (map2d2/pos-xy box start)
        initial-status {:beams {initial-beam 1}
                        :y initial-y}
        maybe-split (fn [x y tls]
                      (if (splitters (map2d2/pos-id box x y))
                        (list [(dec x) tls] [(inc x) tls])
                        (list [x tls])))
        merge-beams (fn [[x xtls]]
                      [x (apply + (map second xtls))])]
    (loop [status initial-status]
      (if (not (map2d2/in-bounds? box [0 (status :y)]))
        (apply + (map second (status :beams)))
        (recur (-> status
                   (update :y inc)
                   (update :beams (fn [beams]
                                    (let [new-beams (mapcat #(maybe-split % (status :y) (get beams % 1)) (keys beams))]
                                      (into {} (map merge-beams (group-by first new-beams))))))))))))

(comment
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
