(ns aoc.aoc2025.day5-1
  (:require [aoc.core :as aoc]
            [clojure.string :as string]))

(def test-input "
3-5
10-14
16-20
12-18

1
5
8
11
17
32
")

(def input-file "data/input-5.txt")

(defn parse-range [line] 
  (mapv parse-long (string/split line #"-")))

(defn dbg [value]
  (println value)
  value)

(defn solution [input] 
  (let [[ranges [_ & sids]] (split-with (comp not empty?) input)
        ids (map parse-long sids)]
    (count (distinct (for [srng ranges
                           id ids
                           :when (let [[lo hi] (parse-range srng)]
                                   (<= lo id hi))]
                       id)))
    ))

(comment
  (solution (aoc/parse-test test-input)) ; must be 3
  ;;
  )

(solution (aoc/read-input input-file))
