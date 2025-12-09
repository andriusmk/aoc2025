(ns aoc.aoc2025.day8-1
  (:require [aoc.core :as aoc]
            [clojure.set :as set]
            [clojure.string :as string]))

(def test-input "Example from AOC" "
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
")

(def input-file "data/input-8.txt")

(defn parse-input [input]
  (mapv (comp (partial mapv parse-long) #(string/split % #",")) input))

(defn distance-sqr [v1 v2]
  (apply + (map (comp #(* % %) -) v1 v2)))

(defn make-pairs [size]
  (for [x (range 0 (dec size))
        y (range (inc x) size)]
    [x y]))

(defn calc-distances [vectors]
  (map #(vector % (apply distance-sqr (map vectors %))) (make-pairs (count vectors))))

(defn connected? [network n1 n2]
  (loop [nodes #{n1}
         prev #{}]
    (cond
      (empty? nodes)  false
      (nodes n2) true
      :else (recur (set/difference (apply set/union (map network nodes)) prev) nodes))))

(defn walk [network node]
  (loop [nodes #{node}
         seen #{}]
    (if (empty? nodes)
      seen
      (recur (set/difference (set (mapcat network nodes)) seen) (set/union seen nodes)))))

(defn circuits [network]
  (loop [cs ()
         known #{}
         [[node _] & remaining :as nodes] network]
    (cond
      (empty? nodes) cs
      (known node) (recur cs known remaining)
      :else (let [new-circuit (walk network node)]
              (recur (conj cs new-circuit)
                     (set/union known new-circuit)
                     remaining)))))

(defn add-connection [network n1 n2]
  (let [connect (fn [net n1' n2']
                  (update net n1' #(conj (or %1 #{}) n2')))]
    (if (connected? network n1 n2)
      network
      (connect network n1 n2))))

(defn update-net [state [box1 box2]]
  (-> state
      (update :network add-connection box1 box2)
      (update :network add-connection box2 box1)))

(defn solution [input]
  (->> input
       parse-input
       calc-distances
       (sort (fn [v1 v2] (< (second v1) (second v2))))
       (take 1000)
       (map first)
       (reduce update-net {:network {}})
       :network
       circuits
       (map count)
       (sort >) 
       (take 3)
       (apply *)
       ))

(comment
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
