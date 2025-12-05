(ns aoc.aoc2025.day5-2
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

(defn get-ranges [input]
  (->> input
       (take-while (comp not empty?))
       (map parse-range)
       (sort (fn [[l1 h1] [l2 h2]]
               (or (< l1 l2)
                   (and (= l1 l2)
                        (< h1 h2)))))))

(defn range-size [lo hi]
  (- (inc hi) lo))

(defn merge-ranges [[ans [l1 h1]] r2]
  (let [[l2 h2] r2]
    (if (>= h1 l2) ; if overlap
      [ans [l1 (max h1 h2)]]
      [(+ ans (range-size l1 h1)) r2])))

(defn solution [input]
  (let [[r1 & ranges] (get-ranges input)
        [ans [lo hi]] (reduce merge-ranges [0 r1] ranges)]
    (dbg [r1 ranges])
    (+ ans (range-size lo hi))))

(comment
  (solution (aoc/parse-test test-input)) ; must be 14
  (solution (aoc/read-input input-file))
  ;;
  )
