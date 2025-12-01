(ns aoc.aoc2025.day3-1
  (:require [aoc.core :refer [parse-test read-input]]
            [clojure.string :as string]))

(def test-input "
987654321111111
811111111111119
234234234234278
818181911112111
")

(def input-file "data/input-3.txt")

(defn parse-bank [text]
  (map (comp parse-long str) text))

(defn max-jolt [bank]
  (letfn [(rec-f [curr b]
             (let [rest-b (rest b)
                   first-b (first b)
                   new-curr (max curr first-b)]
               (cond
                 (empty? rest-b) (+ first-b (* 10 curr))
                 (>= new-curr (apply max rest-b)) (+ (apply max rest-b) (* 10 new-curr))
                 :else (rec-f new-curr rest-b)
                 )))]
    (rec-f 0 bank)))

(defn solution [input]
  (->> input
       (map (comp max-jolt parse-bank)) 
       (apply +)
       ))

(comment
    (solution (parse-test test-input))
  ;;
  )

  (solution (read-input input-file))
