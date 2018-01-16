(ns advent-of-code.day1
  (:require [advent-of-code.core :refer [int-to-digit-list]]))

(defn sum-repeats [n]
  (let [digit-array (int-to-digit-list n)
        digit-array (concat digit-array (list (first digit-array)))]
    (->> digit-array
         (partition 2 1)
         (filter (fn [p] (let [[x y] p] (= x y))))
         (map first)
         (reduce +))))

(defn sum-half-length-repeats [n]
  (let [digit-array (int-to-digit-list n)
        digit-count (count digit-array)
        digit-array (concat digit-array digit-array)
        shift (quot digit-count 2)]
    (->> digit-count
         (range)
         (map (fn [i] (take-nth shift (nthrest digit-array i))))
         (filter (fn [[x y]] (= x y)))
         (map first)
         (reduce +))))
