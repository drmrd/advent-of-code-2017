(ns advent-of-code.day2
  (:require [clojure.string :as str]))

(def puzzle-table
  (->> "resources/day2_input.txt"
       (slurp)
       (str/split-lines)
       (map (comp #(map read-string %)
               #(str/split % (re-pattern "\t"))))))

(def max-diff (comp (partial apply -) (partial apply (juxt max min))))

(defn find-quotient [s]
  (let [sorted (sort s)]
    (loop [remaining (rest sorted)
           divisor (first sorted)]
      (let [multiples (filter #(= (mod % divisor) 0) remaining)]
        (if (not-empty multiples)
          (quot (first multiples) divisor)
          (recur (rest remaining) (first remaining)))))))

(defn checksum [row-fun t] (->> t (map row-fun) (reduce +)))

(def max-diff-checksum (partial checksum max-diff))
(def quotient-checksum (partial checksum find-quotient))

(println (max-diff-checksum puzzle-table))
(println (quotient-checksum puzzle-table))
