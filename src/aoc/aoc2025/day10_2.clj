(ns aoc.aoc2025.day10-2
  (:require [aoc.core :as aoc]
            [clojure.string :as string]))

(def test-input "Example from AOC" "
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
")

(def input-file "data/input-10.txt")

(defn button-effects [size wiring]
  (let [initial (vec (repeat size 0))]
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

(defn clicks-to-joltages [machine clicks]
  (let [effects (fn [b-clicks b-effects] (map (partial * b-clicks) b-effects))
        all-effects (map effects clicks (machine :buttons))]
    (vec (apply map + all-effects))))

(defn calc-score [machine clicks]
  (let [jolts (clicks-to-joltages machine clicks)
        diffs (map (comp abs -) jolts (machine :joltages))
        diff-score (* 1000000 (apply + diffs))]
    (+ diff-score (apply + clicks))))

(defn parse-input [input]
  (map parse-line input))

(defn solve-machine [machine]
  (let [button-cnt (count (machine :buttons))
        initial-clicks (vec (repeat button-cnt 0))
        initial-score (calc-score machine initial-clicks)]
    (loop [clicks initial-clicks
           score initial-score
           forbidden #{}
           stack ()]
      (aoc/dbg [clicks (clicks-to-joltages machine clicks) (machine :joltages) score])
      (let [combinations (for [modifier (range -1 2)
                               index (range 0 button-cnt)
                               :let [new-val (+ (clicks index) modifier)]
                               :when (and (not (zero? modifier)) (not (neg? new-val)))]
                           (let [new-clicks (assoc clicks index new-val)]
                             [(calc-score machine new-clicks) new-clicks]))
            best-combs (sort-by first < combinations)
            best-comb (some #(when (not (forbidden (second %))) %) best-combs)]
        (aoc/dbg (string/join \newline combinations))
        (aoc/dbg (str "best: " best-comb))
        (cond
          (nil? best-comb) (do (aoc/dbg (str "forbidden: " clicks))
                               (aoc/dbg (str "next in stack: " (first stack)))
                               (recur (first stack) score (conj forbidden clicks) (rest stack)))
          (< (first best-comb) score) (recur (second best-comb) (first best-comb) forbidden (conj stack clicks))
          (>= score 1000000) (do
                               (aoc/dbg (str "forbidden: " (second best-comb)))
                               (recur clicks score (conj forbidden (second best-comb)) stack))
          :else (apply + clicks))))))

(defn solution [input]
  (->> input ; strings
       parse-input ; list of machines
       (map solve-machine)
       ;;  (map calc-min-pushes)
       (apply +)))

(comment
  (< nil 1)
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
