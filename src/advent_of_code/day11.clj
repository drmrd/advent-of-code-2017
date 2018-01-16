(ns advent-of-code.day11
  (:require [clojure.string :as str])
  (:use clojure.walk))

(def step-seq (-> "resources/day11_input.txt"
               (slurp)
               (str/trim)
               (str/split #",")
               (#(map keyword %))))

(defn- cubic-taxicab-norm [[x y & z :as point]]
  (case (count point)
    3 (->> point
           (map #(Math/abs %))
           (reduce +)
           (#(quot % 2)))
    2 (cubic-taxicab-norm [x y (+ x y)])))

(defn- cardinal->xy [cards]
  {:x (- (:ne-sw cards) (:nw-se cards))
   :y (+ (:n-s cards) (:nw-se cards))})

(defn- steps->norm [steps]
  (let [step-freqs (frequencies steps)
        keys->axis (fn [k1 k2] (keyword (str (name k1) "-" (name k2))))
        keys->coord (fn [[k1 k2]]
                      [(keys->axis k1 k2) (- (k1 step-freqs) (k2 step-freqs))])
        coords (->> [[:n :s] [:nw :se] [:ne :sw]]
                    (map keys->coord)
                    (flatten)
                    (apply hash-map)
                    (cardinal->xy))]
    (cubic-taxicab-norm (vals coords))))

;; Solution to Part 1
(steps->norm step-seq)                  ; => 705 (Part 1 Solution)

;; Solution to Part 2
(->> step-seq
     (map {:n [0 1] :s [0 -1]
           :ne [1 0] :sw [-1 0]
           :nw [-1 1] :se [1 -1]})
     (reductions #(map + %1 %2) '(0 0))
     (map cubic-taxicab-norm)
     (reduce max))                      ; => 1469 (Part 2 Solution)
