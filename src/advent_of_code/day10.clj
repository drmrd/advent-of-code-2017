(ns advent-of-code.day10
  (:require [advent-of-code.core :refer [file->vec file->bytes]]
            [clojure.string :as str])
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
       (loop [buffer buffer skip skip length-index 0]
         (if (= length-index lengths-count)
           {:lengths lengths :buffer buffer :skip skip}
           (recur (reverse-subseq buffer 0
                                  (nth lengths length-index)
                                  (+ (nth lengths length-index) skip))
                  (inc skip)
                  (inc length-index)))))))

  (is (= (knot-hash-round [3 4 1 5] 5)
         {:lengths [3 4 1 5] :buffer (->CircularBuffer '(3 4 2 1 0) 4) :skip 4}))
  (is (= (knot-hash-* (:buffer (knot-hash-round [3 4 1 5] 5))) 12))
  (is (= (knot-hash-* (:buffer (knot-hash-round lengths 256))) 5577)))

(test #'knot-hash-round)

;;; Part 2

(defn- trim-and-salty-bytes [string salt]
  (-> string
      (str/trim)
      ((partial map byte))
      (concat salt)))

(def salt [17 31 73 47 23])

(defn- sparse-hash
  ([bytes buffer-size] (sparse-hash bytes buffer-size 64))
  ([bytes buffer-size total-rounds]
   (let [rounds (iterate knot-hash-round
                         {:lengths bytes
                          :buffer (->CircularBuffer (vec (range 256)) 0)
                          :skip 0})]
     (.coll (:buffer (nth rounds total-rounds))))))

(defn- dense-hash [bytes buffer-size]
  (let [partitioned-sparse-hash (partition (int (Math/sqrt buffer-size))
                                           (sparse-hash bytes buffer-size))]
    (map (partial apply bit-xor) partitioned-sparse-hash)))

(defn- hex-format-octets [octets]
  (apply str (map #(format "%02x" %) octets)))

(with-test
  (defn- knot-hash
    ([s] (knot-hash s 256 [17 31 73 47 23]))
    ([s buffer-size salt]
     (-> s
         (trim-and-salty-bytes salt)
         (dense-hash buffer-size)
         (hex-format-octets))))

  (is (= (knot-hash "") "a2582a3a0e66e6e86e3812dcb672a272"))
  (is (= (knot-hash "AoC 2017") "33efeb34ea91902bb2f59c9920caa6cd"))
  (is (= (knot-hash "1,2,3") "3efbe78a8d82f29979031a4aa0b16a9d"))
  (is (= (knot-hash "1,2,4") "63960835bcdc130f0b66d7ff4f6a5a8e"))
  (is (= (knot-hash (slurp "resources/day10_input.txt"))
         "44f4befb0f303c0bafd085f97741d51d")))

(test #'knot-hash)
