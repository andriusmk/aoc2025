(ns aoc.aoc2025.day10-2
  (:require [aoc.core :as aoc]
            [clojure.string :as string]))

(def test-input "Example from AOC" "
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
")

(def input-file "data/input-10.txt")

(defn parse-lights [field]
  (let [pattern (subs field 1 (dec (count field)))
        light-set (->> pattern
                       (map (fn [idx c]
                              (if (= c \#)
                                (bit-shift-left 1 idx)
                                0))
                            (range))
                       (apply bit-or))]
    [light-set, (count pattern)]))

(defn button-effects [size wiring]
  (let [ initial (vec (repeat size 0))]
    (reduce #(assoc %1 %2 1) initial wiring)))

(defn parse-button [field]
  (as-> field <>
    (subs <> 1 (dec (count field)))
    (string/split <> #",")
    (map parse-long <>)))

(defn parse-line [line]
  (let [fields (string/split line #"\s+")
        buttons (drop-last (rest fields))
        joltages (vec (parse-button (last fields)))]
    {:joltages joltages
     :buttons (mapv (comp (partial button-effects (count joltages)) parse-button) buttons)}))

(defn press-button [device joltages n]
  (let [effects ((device :buttons) n)
        target (device :joltages)]
    (loop [state joltages [curr & remaining :as curr-effects] effects]
      (cond
        (empty? curr-effects) state
        (= (state curr) (target curr)) nil
        :else (recur (update state curr inc) remaining)))))

(defn iterate-state [device joltages]
  (let [all-buttons (device :buttons)]
    (->> all-buttons
         count
         (range)
         (map (partial press-button device joltages))
         (remove nil?))))

(defn calc-min-pushes [device]
  (aoc/dbg device)
  (let [target (device :joltages)]
    (loop [states (list (vec (repeat (count target) 0))) n 0]
      (cond
        (empty? states) nil
        (some (partial = target) states) n
        :else (recur (mapcat (partial iterate-state device) states) (inc n))))))

(defn parse-input [input]
  (map parse-line input))

(defn solution [input]
  (->> input
       parse-input
       ;;  (map calc-min-pushes)
       ;;  (apply +)
       ))

(comment
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
