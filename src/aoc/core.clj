(ns aoc.core (:require [clojure.string :as string]))

(defn parse-test
  "Splits input into lines of text preserving all whitespace
   and removing leading and trailing empty lines."
  [input]
  (->> input
       string/split-lines
       (drop-while empty?)
       (take-while (comp not empty?))
       vec))

(defn read-input
  "Reads the input file, splits the text into lines,
   removes leading and trailing empty lines."
  [name]
  (parse-test (slurp name)))

(defn dbg
  "Prints the parameter and evaluates to it."
  [& args]
  (let [value (last args)]
    (apply println args)
    value))

(defn split-all
  "Splits the given collection on all occurrences of an item
   for which the predicate `what` returns `false`. Those
   items are not included in the output sequence."
  [what coll] 
  (reverse (loop [args' coll
                  res ()]
             (if (empty? args') res
                 (let [[grp [_ & remaining]] (split-with what args')]
                   (recur remaining (conj res grp)))))))

(comment
  ;
  )