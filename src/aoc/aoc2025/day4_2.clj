(ns aoc.aoc2025.day4-2
  (:require [aoc.core :as aoc]
            [aoc.map2d :as map2d]
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

(defn get-occupied [scene]
  (set (map first (filter (fn [[_ c]] (= c \@)) (map2d/enumerate scene)))))

(defn solution [input]
  (let [scene (map2d/create-map input)
        neighbours (fn [pos] (let [[ px py] (map2d/pos-xy scene pos)]
                               (set (for [x (range -1 2)
                                          y (range -1 2)
                                          :when (not (= x y 0))]
                                      (map2d/pos-id scene (+ px x) (+ py y))))))
        neighb-cnt (fn [occupied pos] (count (set/intersection (neighbours pos) occupied)))
        valid? (fn [occupied pos] (< (neighb-cnt occupied pos) 4))
        find-valid (fn [occupied] (set (filter (partial valid? occupied) occupied)))]
    (loop [occupied (get-occupied scene)
           ans 0]
      (let [removable (find-valid occupied)
            rcnt (count removable)]
        ; (println rcnt)
        (if (zero? rcnt) ans
            (recur (set/difference occupied (set removable)) (+ ans rcnt)))))))

(comment
  (solution (aoc/parse-test test-input)) ; must be 43
  (time (solution (aoc/read-input input-file)))
  ;;
  )
