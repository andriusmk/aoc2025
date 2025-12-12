(ns aoc.aoc2025.day10-2
  (:require [aoc.core :as aoc]
            [clojure.string :as string]
            [clojure.set :as set]))

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
        diff-score (apply + diffs)]
    diff-score))

(defn parse-input [input]
  (map parse-line input))

(defn make-new-combinations [machine clicks]
  (let [button-cnt (count (machine :buttons))]
  ;;   (loop [depth 0 mods ()]
  ;;     (let [new-mods ]
  ;;       (cond
  ;;         (= depth button-cnt) mods
  ;;         :else (recur (inc depth) mods))))
    (for [modifier (range -1 2)
          modifier-2 (range -1 2)
          index (range 0  button-cnt)
          index-2 (range 0 button-cnt)
          :let [new-val (+ (clicks index) modifier)
                new-val-2 (+ (clicks index-2) modifier-2)]
          :when (and (< index-2 index) (not (= modifier modifier-2 0)) (not (neg? new-val)) (not (neg? new-val-2)))]
      (let [new-clicks (-> clicks
                           (assoc index new-val)
                           (assoc index-2 new-val-2))]
        [(calc-score machine new-clicks) new-clicks]))))

(defn solve-machine [machine]
  (let [button-cnt (count (machine :buttons))
        initial-clicks (vec (repeat button-cnt 0))
        initial-score (calc-score machine initial-clicks)]
    (loop [combinations `([~initial-score ~initial-clicks])
           visited #{}
           found-good #{}]
      ;; (aoc/dbg "Combinations:" combinations)
      (let [better? (fn [current new]
                      (and (>= (first current) (first new)) (not (visited (second new)))))
            new-combinations (distinct (mapcat (fn [combination]
                                                 (->> combination
                                                      second
                                                      (make-new-combinations machine)
                                                      distinct
                                                      (filter (partial better? combination))
                                                      ))
                                               combinations))
            good-ones (map second (filter (comp zero? first) new-combinations)) 
            min-score (if (seq new-combinations) (first (apply min-key first new-combinations)) nil)
            filtered (when (seq new-combinations) (filter (comp (partial = min-score) first) new-combinations))
            ]
        (aoc/dbg "Min score:" min-score)
        ;; (aoc/dbg "Filtered:" filtered)
        (aoc/dbg "Foud good:" found-good)
        (cond
          (empty? filtered) (apply min (map (partial apply +) found-good))
          ; (seq good-ones) (recur good-ones (set/union visited (set (map second combinations))) (set/union found-good (set (map second good-ones))))
          :else (recur filtered (set/union visited (set (map second combinations))) (set/union found-good (set good-ones)))
          )))))

(defn solution [input]
  (->> input ; strings
       parse-input ; list of machines
       ; first
       ; solve-machine
       (map (comp aoc/dbg solve-machine))
       ;; (map calc-min-pushes)
       (apply +)
       ))

(comment
  (< nil 1)
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
