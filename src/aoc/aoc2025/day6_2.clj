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
  (let [[_ n op] (re-find #"(\d+)|([+*])" item)]
    (cond
      n  (parse-long n)
      op (case op
           "+" +
           "*" *))))

(defn solution [input]
  (let [ops (as-> (last input) <>
              (string/trim <>)
              (string/split <> #"\s+")
              (map parse-item <>))
        args (take (dec (count input)) input)]
    (->> args
         (apply map (comp parse-item str))
         (aoc/split-all identity)
         (map apply ops)
         (apply +))))

(comment
  (solution (aoc/parse-test test-input))
  (time (solution (aoc/read-input input-file)))
  ;;
  )
