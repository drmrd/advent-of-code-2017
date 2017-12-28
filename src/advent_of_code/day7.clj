(ns advent-of-code.day7
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defrecord Program [^String name ^int weight ^java.util.List children])

(defn- note->Program [note]
  (let [[name weight children] (-> note
                                   (str/replace #"\(|\)" "")
                                   (str/split #"\s+(:?->)?\s*" 3))]
    (->Program name
               (read-string weight)
               (if-not children [] (str/split children #", ")))))

(defn parse-tower-notes [notes]
  (map note->Program (str/split-lines notes)))

(defn- child? [potential-child program]
  (boolean (some #{(:name potential-child)} (:children program))))

(defn- bottom-program? [program programs]
  (not (some (partial child? program) programs)))

(defn bottom-program [programs]
  (first (filter #(bottom-program? % programs) programs)))

(def tower-data (-> "resources/day7_input.txt" (slurp) (parse-tower-notes)))
(def day7-part1-solution (:name (bottom-program tower-data)))
