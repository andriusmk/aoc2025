(ns aoc.map2d 
  (:refer-clojure :exclude [get])
  (:require [clojure.string :as string]
            [aoc.core :as aoc]))

(defn create-map [input]
  {:width (count (input 0)) 
   :data (vec (apply str input))})

(defn pos-id [{width :width} x y]
  (if (<= 0 x (dec width)) (+ (* y width) x) nil))

(defn pos-xy [{width :width} id]
  [(mod id width) (quot id width)])

(defn get
  ([{:keys [data] :as m} pos c]
   (cond
     (vector? pos) (clojure.core/get data (apply (partial pos-id m) pos) c)
     (number? pos) (clojure.core/get data pos c)
     :else c))
  ([m pos] (get m pos nil)))

(defn check [m pos c]
  (= (get m pos) c))

(defn enumerate [m]
  (map vector (range) (m :data)))

(defn set-to [m c & positions]
  (reduce (fn [m' pos]
            (if (<= 0 pos (dec (count (:data m'))))
              (update m' :data #(assoc % pos c))
              m')) m positions))

(defn show [m]
  (string/join \newline (aoc/chop (:width m) (:data m))))

(comment
  (let [m (create-map ["@.@" ".@." "@@@"])]
    (println (show (set-to m \. (pos-id m 0 2))))
    (println (get m 9 \.)))
  ;;
  )