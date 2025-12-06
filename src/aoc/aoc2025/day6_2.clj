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
  (let [ops (as-> (last input) <>
              (string/trim <>)
              (string/split <> #"\s+")
              (map parse-item <>))
        args (take (dec (count input)) input)]
    (as-> args <>
      (apply map (comp parse-item
                       string/trim
                       str) <>)
      (aoc/split-all identity <>)
      (map conj <> ops)
      (conj <> '+)
      (eval <>))))

(comment
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
