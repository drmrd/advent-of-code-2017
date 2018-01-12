(ns advent-of-code.day10
  (:require [advent-of-code.core :refer [file->vec]])
  (:require [advent-of-code.circular-buffer :refer :all])
  (:import [advent_of_code.circular_buffer CircularBuffer]))

(def lengths (file->vec "resources/day10_input.txt"))

(with-test
  (defn knot-hash [lengths buffer-size]
    (let [lengths-count (count lengths)]
      (loop [buffer (->CircularBuffer (vec (range buffer-size)) 0)
             skip 0]
        (if (= skip lengths-count)
          (->> buffer (.coll) ((juxt first second)) (reduce *))
          (recur (reverse-subseq buffer 0
                                 (nth lengths skip)
                                 (+ (nth lengths skip) skip))
                 (inc skip))))))
  (is (= (knot-hash [3, 4, 1, 5] 5) 12))
  (is (= (knot-hash lengths 256) 5577)))

(def part1-solution (knot-hash lengths 256))
