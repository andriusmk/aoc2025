(ns aoc.map2d
  (:refer-clojure :exclude [get])
  (:require [clojure.string :as string]))

(defn create-map [input]
  {:width (count (input 0))
   :data input})

(defn pos-id [{width :width} x y]
  (if (<= 0 x (dec width)) (+ (* y width) x) nil))

(defn pos-xy [{width :width} id]
  [(mod id width) (quot id width)])

(defn enumerate [m]
  (map vector (range) (string/join (m :data))))

(defn dump [m]
  (string/join (m :data)))

(defn show [m]
  (string/join \newline (m :data)))

(comment
  ;;
  )
