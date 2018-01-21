(ns advent-of-code.day14
  (:require [advent-of-code.day10 :refer [knot-hash]]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer [cl-format]]
            [criterium.core :as criterium]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(def key-string "wenycdww")

(defn- hash-inputs [key-string]
  (map str (repeat key-string) (repeat "-") (range 128)))

(defn- hex-str->bit-list [hex]
  (let [hex-char->bit-list (comp (partial map #(Integer/parseInt (str %)))
                              (partial cl-format nil "~4,'0',B")
                              #(Integer/parseInt % 16)
                              str)]
    (->> hex (map hex-char->bit-list) (flatten))))

(defn- binary-hashes [key-string]
  (->> key-string (hash-inputs) (map (comp hex-str->bit-list knot-hash))))

;; (reduce + (flatten (binary-hashes key-string))) ; => 8226 (Part 1 Solution)

(defn- contiguous-used-regions [key-string]
  (let [memory (binary-hashes key-string)
        used? (fn [[i j]] (-> memory (nth i) (nth j) (pos?)))
        address? (fn [coords] (->> coords
                                  (map #(and (>= % 0) (< % 128)))
                                  (apply #(and %1 %2))))
        used-pairs (->> 128
                        (range)
                        (#(combo/cartesian-product % %))
                        (filter used?))
        used-neighbors
        (let [adjacency-shifts [[0 1] [0 -1] [-1 0] [1 0]]]
          (fn [pair] (filter #(and (address? %) (used? %))
                            (map #(map + pair %) adjacency-shifts))))
        used-adjacent-pairs
        (distinct (apply concat (map (fn [p] (map #(vector p %)
                                                 (used-neighbors p)))
                                     used-pairs)))
        g (apply uber/graph used-adjacent-pairs (concat used-pairs
                                                        used-adjacent-pairs))]
    (count (alg/connected-components g))))

;; (contiguous-used-regions key-string)    ; => 1128 (Part 2 Solution)
