(ns advent-of-code.day5
  (:require [advent-of-code.core :refer [file->vec]]))

(def init-maze (file->vec "resources/day5_input.txt"))

(defn- aget-and-fn [f]
  (fn [int-arr idx]
    (let [val (aget int-arr idx)]
      (aset int-arr idx (f val))
      val)))

(defn- steps-to-escape-maze [init-maze jump-rule]
  (let [maze (int-array init-maze)
        maze-length (count maze)]
    (loop [index 0 step 0]
      (if (>= index maze-length)
        step
        (recur (+ index (jump-rule maze index)) (inc step))))))

(defn- answer-part1 [init-maze]
  (steps-to-escape-maze init-maze (aget-and-fn inc)))

(defn- answer-part2 [init-maze]
  (steps-to-escape-maze init-maze
                        (aget-and-fn #(if (< % 3) (inc %) (dec %)))))

(def sample-maze [0 3 0 1 -3])
(assert (= (answer-part1 sample-maze) 5))
(assert (= (answer-part2 sample-maze) 10))

(answer-part1 init-maze)
(answer-part2 init-maze)
