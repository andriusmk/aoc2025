(ns aoc.aoc2025.day6-1
  (:require [aoc.core :as aoc]
            [clojure.string :as string]))

(def test-input "
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
")

(def input-file "data/input-6.txt")

(defn parse-item [item]
  (cond
    (re-matches #"\d+" item) (parse-long item)
    (re-matches #"[+*]" item) (symbol item)))

(defn parse-line [line]
  (as-> line <>
    (string/trim <>)
    (string/split <> #"\s+")
    (map parse-item <>)
    ))

(defn solution [input]
  (->> input
       (map parse-line)
       (apply map list)
       (map (comp eval reverse))
       (apply +)
       ))

(comment
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
