(ns aoc.core (:require [clojure.string :as string]
                       [clojure.java.io :refer [reader]]))

(defn parse-test [input]
  (vec (remove empty? (string/split-lines input))))

(defn read-input [name] (parse-test (slurp name)))

(comment
  (defn read-input [name]
    (remove empty? (with-open [rdr (reader name)]
                     (doall (line-seq rdr)))))
  )