(ns aoc.aoc2025.day9-2
  (:require [aoc.core :as aoc]
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

(defn area [v1 v2]
  (apply * (map (comp inc abs -) v1 v2)))

(defn flip-v [[x1 y1] [x2 y2]]
  [[x1 y2] [x2 y1]])

(defn flip-h [[x1 y1] [x2 y2]]
  [[x2 y1] [x1 y2]])

(defn pos-rec [vectors t1 t2]
  (let [[[x1 y1 :as v1] [x2 y2 :as v2]] (mapv vectors [t1 t2])]
    (cond->> [v1 v2]
      (< x1 x2) (apply flip-v)
      (< y1 y2) (apply flip-h))))

(defn inside? [vectors [t1 t2] t]
  (let [[[x1 x2] [y1 y2]] (pos-rec vectors t1 t2)
        [x y] t]
    (and (> x1 x x2)
         (> y1 y y2))))

(defn make-pairs [size]
  (for [x (range 0 (dec size))
        y (range (inc x) size)]
    [x y]))

(defn calc-areas [vectors]
  (map #(vector % (apply area (map vectors %))) (make-pairs (count vectors))))

(defn compare-areas [vectors p1 p2]
  (let [[v1 v2] (map vectors p1)
        [v1' v2'] (map vectors p2)
        a1 (area v1 v2)
        a2 (area v1' v2')
        ] (> a1 a2))
  )

(defn init-env [input]
  (let [vectors (parse-input input)
        all-pairs (make-pairs (count input))
        first-dx (- ((vectors 1) 0) ((vectors 0) 0))
        vert-idx (if (zero? first-dx) 0 1)
        hor-idx (mod (inc vert-idx) 2)
        ]
    {:vectors vectors
     :sorted-pairs (sort (partial compare-areas vectors) all-pairs)
     :verticals (vec (sort-by (comp second vectors) < (range vert-idx (count vectors) 2)))
     :horizontals (vec (sort-by (comp first vectors) < (range hor-idx (count vectors) 2)))}))

(defn valid? [env rect]
  )

(defn stats [vectors]
  (->> vectors
       (reduce (fn [m [x y]]
                 ; (aoc/dbg [x m])
                 (update m x #(conj % y)))
               {})
       vals
       (map (partial apply (comp abs -)))
       (apply min)
       ))

(defn solution [input]
  (let [env (init-env input)]
    env)
  )

(comment
  (stats [[1 1] [1 2] [1 3]])
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  (update {} 1 identity)
  ;;
  )
