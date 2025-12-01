(ns aoc.aoc2025.day1-1
  (:require [aoc.core :refer [parse-test read-input]]))

(def test-input "
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
")

(def input-file "data/input-1.txt")

(defn parse-rotation [code]
  (let [[_ dir param] (re-matches #"([LR])(\d+)" code)
        steps (parse-long param)]
    (case dir
      "R" steps
      "L" (- steps))))

(defn turn [cmd position]
  (let [steps (parse-rotation cmd)]
    (mod (+ position steps 100) 100)))

(defn solution [lines]
  (let [red-func (fn [history cmd]
                   (->> history
                        first
                        (turn cmd)
                        (conj history)))]

    (->> lines
         (reduce red-func '(50))
         (filter zero?)
         count)))

(comment
  (solution (parse-test test-input))

  ;; end of comments
  )

(solution (read-input input-file))
