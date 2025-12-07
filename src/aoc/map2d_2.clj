(ns aoc.map2d-2
  (:refer-clojure :exclude [get])
  (:require [clojure.string :as string]))

(defn create-map [input]
  {:box {:width (count (input 0))
         :height (count input)}
   :data input})

(defn get-box [m]
  (m :box))

(defn in-bounds? [box [x y]]
  (and (<= 0 x (dec (box :width)))
       (<= 0 y (dec (box :height)))))

(defn pos-id
  ([box [x y :as posxy]]
   (if (in-bounds? box posxy)
     (+ (* y (box :width)) x)
     nil))
  ([box x y]
   (pos-id box [x y])))

(defn pos-xy [{width :width} id]
  [(mod id width) (quot id width)])

(defn rel-pos [box pos [dx dy]]
  (let [[px py] (pos-xy box pos)
        x (+ px dx)
        y (+ py dy)]
    (pos-id box x y)))

(defn enumerate [m]
  (map vector (range) (string/join (m :data))))

(defn dump [m]
  (string/join (m :data)))

(defn show [m]
  (string/join \newline (m :data)))

(comment
  (seq [2])
  ;;
  )
