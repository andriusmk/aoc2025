(ns aoc.aoc2025.day2-1
  (:require [aoc.core :refer [parse-test read-input]]
            [clojure.string :as string]))

(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(def input-file "data/input-2.txt")

(defn parse-input [text] (string/split text #","))

(defn parse-range [text] (mapv parse-long (string/split text #"-")))

(defn ndigits [value] (if (zero? value) 0 (inc (ndigits (quot value 10)))))

(defn long-exp10 [value] (first (drop value (iterate (partial * 10) 1))))

(defn split-number [value]
  (let [n (long-exp10 (quot (ndigits value) 2))]
    [(quot value n) (mod value n)]))

(defn duplicate [value]
  (+ (* (long-exp10 (ndigits value)) value) value))

(defn count-ids-of [lo hi]
  (let [nlo (ndigits lo)
        nhi (ndigits hi)
        lo' (if (odd? nlo) (long-exp10 nlo) lo)
        hi' (if (odd? nhi) (dec (long-exp10 (dec nhi))) hi)
        [lo-left _] (split-number lo')
        [hi-left _] (split-number hi')]
    (apply + (filter #(<= lo % hi) (map duplicate (range lo-left (inc hi-left)))))))

;; That's not beautiful but who cares
(defn count-ids [[lo hi]]
  (let [nlo (ndigits lo)
        nhi (ndigits hi)]
    (cond
      (and (odd? nlo) (= nlo nhi)) 0
      :else (count-ids-of lo hi))))

(defn solution [input]
  (apply + (map (comp count-ids parse-range) (parse-input input))))

(comment
  (solution test-input)
  )

(solution (string/trim (slurp input-file)))
