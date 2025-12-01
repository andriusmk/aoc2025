(ns aoc.aoc2025.day1-2
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
  (let [steps (parse-rotation cmd)
        screwed-pos (+ position steps)
        turns (quot screwed-pos 100)
        new-pos (mod screwed-pos 100)]
    
    [new-pos (cond
               (pos? screwed-pos) turns
               (neg? screwed-pos) (if (zero? position)
                                    (- turns)
                                    (inc (- turns)))
               :else 1)]))

(defn solution [lines]
  (let [red-func (fn [[position x0s] cmd]
                   (let [[new-pos turns] (turn cmd position)]
                     [new-pos (+ x0s turns)]))] 
    (second (reduce red-func [50 0] lines))))

(comment 
  (solution (parse-test test-input))
  ;; end of comments
  )

(solution (read-input input-file))