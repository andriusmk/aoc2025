(ns aoc.aoc2025.day2-2
  (:require [aoc.core :refer [parse-test read-input]]
            [clojure.string :as string]))

(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(def input-file "data/input-2.txt")

(defn parse-input [text] (string/split text #","))

(defn parse-range [text] (mapv parse-long (string/split text #"-")))

(defn ndigits [value] (if (zero? value) 0 (inc (ndigits (quot value 10)))))

(defn long-exp10 [value] (nth (iterate (partial * 10) 1) value))

(defn top [n value] (quot value (long-exp10 (- (ndigits value) n))))

(defn repeat-num [n value]
  (let [scaler (long-exp10 (ndigits value))]
    (nth (iterate #(+ (* % scaler) value) 0) n)))

(defn top-ranges [lo hi]
  (let [nd (ndigits lo)]
    (->> (range 1 (inc (quot nd 2)))
         (filter #(zero? (mod nd %)))
         (map #(vector (top % lo) (top % hi) (quot nd %))))))

(defn proc-top-range [lo hi n]
  (->> (range lo (inc hi))
       (map (partial repeat-num n))))

(defn proc-good-range [lo hi]
  (->> (top-ranges lo hi)
       (map (partial apply proc-top-range))
       flatten
       (filter #(<= lo % hi))
       set
       (apply +)))

(defn proc-range [lo hi]
  (let [nlo (ndigits lo)
        nhi (ndigits hi)]
    (if (= nlo nhi)
      (proc-good-range lo hi)
      (+ (proc-good-range lo (dec (long-exp10 (ndigits lo))))
         (proc-good-range (long-exp10 (dec (ndigits hi))) hi)))))

(defn solution [input]
  (apply + (map (comp (partial apply proc-range) parse-range) (parse-input input))))

(comment
  (solution test-input)
  )

(solution (string/trim (slurp input-file)))
