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

(defn flip-v [[[x1 y1] [x2 y2]]]
  [[x1 y2] [x2 y1]])

(defn flip-h [[[x1 y1] [x2 y2]]]
  [[x2 y1] [x1 y2]])

(defn pos-rec [vectors rect]
  (let [[[x1 y1 :as v1] [x2 y2 :as v2]] (mapv vectors rect)]
    (cond->> [v1 v2]
      (> x1 x2) flip-h
      (> y1 y2) flip-v)))

(defn make-pairs [size]
  (for [x (range 0 (dec size))
        y (range (inc x) size)]
    [x y]))

(defn compare-areas [vectors p1 p2]
  (let [[v1 v2] (map vectors p1)
        [v1' v2'] (map vectors p2)
        a1 (area v1 v2)
        a2 (area v1' v2')] (> a1 a2)))

(defn init-env [input]
  (let [vectors (parse-input input)
        all-pairs (make-pairs (count input))
        first-dx (- ((vectors 1) 0) ((vectors 0) 0))
        vert-idx (if (zero? first-dx) 0 1)
        hor-idx (mod (inc vert-idx) 2)]
    {:vectors vectors
     :sorted-pairs (sort (partial compare-areas vectors) all-pairs)
     :verticals (vec (sort-by (comp second vectors) < (range vert-idx (count vectors) 2)))
     :horizontals (vec (sort-by (comp first vectors) < (range hor-idx (count vectors) 2)))}))

(defn intersects-v? [vectors [[xlo ylo] [xhi yhi] :as rect] idx]
  (let [[x y1] (vectors idx)
        [_ y2] (vectors (mod (inc idx) (count vectors)))]
    (and (< xlo x xhi)
         (or (< ylo y1 yhi)
             (< ylo y2 yhi)
             (and (<= yhi (max y1 y2))
                  (>= ylo (min y1 y2)))))))

(defn intersects-h? [vectors [[xlo ylo] [xhi yhi]] idx]
  (let [[x1 y] (vectors idx)
        [x2 _] (vectors (mod (inc idx) (count vectors)))]
    (and (< ylo y yhi)
         (or (< xlo x1 xhi)
             (< xlo x2 xhi)
             (and (<= xhi (max x1 x2))
                  (>= xlo (min x1 x2)))))))

(defn valid? [env rect]
  (let [vectors (env :vectors)
        raw-rect (pos-rec vectors rect)]
    (if (or (some (partial intersects-v? vectors raw-rect) (env :verticals))
            (some (partial intersects-h? vectors raw-rect) (env :horizontals)))
      nil
      rect)))

(defn solution [input]
  (let [env (init-env input)]
    (apply area (pos-rec (env :vectors) (some (partial valid? env) (env :sorted-pairs))))))

(comment
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
