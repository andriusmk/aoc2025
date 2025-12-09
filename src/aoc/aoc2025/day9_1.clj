(ns aoc.aoc2025.day9-1
  (:require [aoc.core :as aoc]
            [clojure.set :as set]
            [clojure.string :as string]))

(def test-input "Example from AOC" "
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
")

(def input-file "data/input-9.txt")

(defn parse-input [input]
  (mapv (comp (partial mapv parse-long) #(string/split % #",")) input))

(defn distance-sqr [v1 v2]
  (apply + (map (comp #(* % %) -) v1 v2)))

(defn area [v1 v2]
  (apply * (map (comp inc abs -) v1 v2)))

(defn make-pairs [size]
  (for [x (range 0 (dec size))
        y (range (inc x) size)]
    [x y]))

(defn calc-distances [vectors]
  (map #(vector % (apply distance-sqr (map vectors %))) (make-pairs (count vectors))))

(defn calc-areas [vectors]
  (map #(vector % (apply area (map vectors %))) (make-pairs (count vectors))))

(defn count-disconnected [network n1 n2]
  (loop [nodes #{n1}
         prev #{}
         node-cnt 0]
    (cond
      (empty? nodes) node-cnt
      (nodes n2) nil
      :else (recur (set/difference (apply set/union (map network nodes)) prev) nodes (+ node-cnt (count nodes))))))

(defn connect-count [network [n1 n2]]
  (let [n1-n2 (count-disconnected network n1 n2)]
    (if n1-n2
      (let [n2-n1 (count-disconnected network n2 n1)]
        [(-> network
             (update n1 #(conj (or % #{}) n2))
             (update n2 #(conj (or % #{}) n1)))
         (+ n1-n2 n2-n1)])
      [network nil])))

(defn connect-full [total-size pairs]
  (loop [net {} [curr & remaining :as connections] pairs]
    (let [[net' curr-size] (connect-count net curr)]
      (cond
        (empty? connections) nil
        (= curr-size total-size) curr
        :else (recur net' remaining)))))

(defn solution [input]
  (let [vectors (parse-input input)]
    (->> vectors
         calc-areas
         (apply max-key second)
         second
         )))

(comment
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
