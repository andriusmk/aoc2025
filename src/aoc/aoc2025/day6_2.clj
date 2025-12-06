(ns aoc.aoc2025.day6-2
  (:require [aoc.core :as aoc]
            [clojure.string :as string]))

(def test-input "Example from AOC" "
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

(defn solution [input]
  (let [ops (map parse-item (string/split (string/trim (last input)) #"\s+"))
        args (take (dec (count input)) input)]
    (as-> args <>
      (apply map (comp parse-item string/trim str) <>)
      (aoc/split-all identity <>)
      (map (comp eval conj) <> ops)
      (apply + <>))))

(comment
  (solution (aoc/parse-test test-input))
  (solution (aoc/read-input input-file))
  ;;
  )
