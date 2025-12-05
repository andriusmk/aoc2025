(ns aoc.core (:require [clojure.string :as string]))

(defn parse-test [input]
  (string/split-lines (string/trim input)))

(defn read-input [name] (parse-test (slurp name)))

(defn dbg [value]
  (println value)
  value)

(defn chop [n s]
  (loop [parts () s' s]
    (if (empty? s') (->> parts
                         reverse
                         (map (partial apply str)))
        (let [[fst snd] (split-at n s')]
          (recur (conj parts fst) snd)))))

(comment
  ;
  )