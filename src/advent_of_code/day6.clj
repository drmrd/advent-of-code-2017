(ns advent-of-code.day6
  (:require [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def sample-memory [0 2 7 0])
(def memory-array (->> "resources/day6_input.txt"
                       (slurp)
                       (#(concat "[" % "]"))
                       (apply str)
                       (read-string)))

(defn- max-info [arr]
  "Return the index and value of the maximum value in a given array."
  (let [max-val (apply max arr)] [(.indexOf arr max-val) max-val]))

(defn- reallocate [memory length]
  (let [[index-of-max max-val] (max-info memory)
        blocks-for-all (quot max-val length)
        remaining-blocks (mod max-val length)
        banks-at-end (min remaining-blocks (- (dec length) index-of-max))
        banks-at-start (max (- remaining-blocks banks-at-end) 0)
        new-banks (transient [])]

    (dorun
     (for [i (range length)
           :let [current-val (nth memory i)
                 shifted-val (+ (cond
                                  (and (> i index-of-max) (= current-val max-val))
                                  current-val
                                  :else (mod current-val max-val))
                                blocks-for-all)]]
       (conj! new-banks
              (if (or (< i banks-at-start)
                      (and (> i index-of-max)
                           (< i (+ index-of-max banks-at-end 1))))
                (inc shifted-val)
                shifted-val))))
    (persistent! new-banks)))

(assert (= (reallocate [0 2 7 0] 4) [2 4 1 2]))
(assert (= (reallocate [2 4 1 2] 4) [3 1 2 3]))
(assert (= (reallocate [3 1 2 3] 4) [0 2 3 4]))
(assert (= (reallocate [0 2 3 4] 4) [1 3 4 1]))
(assert (= (reallocate [1 3 4 1] 4) [2 4 1 2]))

(def non-empty? (comp not empty?))

(defn- element? [element set]
  (non-nil? (set element)))

(defn- count-redistribution-cycles [memory-seq]
  (let [history (transient #{})
        bank-count (count memory-seq)]
    (loop [cycles 0 memory memory-seq]
      (if (element? memory history)
        cycles
        (do (conj! history memory)
            (recur (inc cycles) (reallocate memory bank-count)))))))

(assert (= (count-redistribution-cycles [0 2 7 0]) 5))
(count-redistribution-cycles memory-array)
