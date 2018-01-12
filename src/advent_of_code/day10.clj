(ns advent-of-code.day10
  (:require [advent-of-code.core :refer [file->vec file->bytes]])
  (:require [advent-of-code.circular-buffer :refer :all])
  (:require [clojure.test :refer :all])
  (:import [advent_of_code.circular_buffer CircularBuffer]))

;;; Part 1

(def lengths (file->vec "resources/day10_input.txt"))

(defn- knot-hash-* [cb] (->> cb (.coll) ((juxt first second)) (reduce *)))

(with-test
  (defn knot-hash-round
    ([lengths buffer-size]
     (knot-hash-round {:lengths lengths
                       :buffer (->CircularBuffer (vec (range buffer-size)) 0)
                       :skip 0}))
    ([{:keys [lengths buffer skip]}]
     (let [lengths-count (count lengths)]
       (loop [buffer buffer skip skip]
         (if (= skip lengths-count)
           {:lengths lengths :buffer buffer :skip skip}
           (recur (reverse-subseq buffer 0
                                  (nth lengths skip)
                                  (+ (nth lengths skip) skip))
                  (inc skip)))))))
  (is (= (knot-hash-round [3 4 1 5] 5)
         {:lengths [3 4 1 5] :buffer (->CircularBuffer '(3 4 2 1 0) 4) :skip 4}))
  (is (= (knot-hash-* (:buffer (knot-hash-round [3 4 1 5] 5))) 12))
  (is (= (knot-hash-* (:buffer (knot-hash-round lengths 256))) 5577)))

(test #'knot-hash-round)

(def part1-solution (knot-hash-* (:buffer (knot-hash-round lengths 256))))

;;; Part 2

(def input-bytes (file->bytes "resources/day10_input.txt"))
(def salt [17, 31, 73, 47, 23])
(def bytes (concat input-bytes salt))
