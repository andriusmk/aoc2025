(ns aoc.aoc2025.day3-2
  (:require [aoc.core :refer [parse-test read-input]]))

(def test-input "
987654321111111
811111111111119
234234234234278
818181911112111
")

(def input-file "data/input-3.txt")

(defn max-jolt [line]
  (let [pairs (map vector (range) line)
        total-len (count line)
        rec-f (fn rec-f [n jolts from]
                (if (zero? n) jolts
                    (let [[idx c] (->> pairs
                                       (filter (fn [[idx, _]] (<= from idx (- total-len n))))
                                       (sort (fn [[i1 c1] [i2 c2]] 
                                               (or (> (int c1) (int c2)) 
                                                   (and (= (int c1) (int c2)) (< i1 i2)))))
                                       first)]
                      (rec-f (dec n) (str jolts c) (inc idx)))))]
    (parse-long (rec-f 12 "" 0))))

(defn solution [input]
  (->> input
       (map max-jolt)
       (apply +)
       ))

(comment
  (solution (parse-test test-input))
  
  (max-jolt "987654321111111")
  (max-jolt "811111111111119")
  (max-jolt "234234234234278")
  (max-jolt "818181911112111")
  ;;
  )

(solution (read-input input-file))
