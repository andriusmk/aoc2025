(ns aoc.core (:require [clojure.string :as string]))

(defn parse-test [input]
  (vec (drop-while empty? (string/split-lines (string/trimr input)))))

(defn read-input [name] (parse-test (slurp name)))

(defn dbg [value]
  (println value)
  value)

(comment
  ;
  )