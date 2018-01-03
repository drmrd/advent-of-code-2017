(ns advent-of-code.day9
  (:require [clojure.string :as str]))

(def stream (slurp "resources/day9_input.txt"))

(defn- scrub-bangs [s] (str/replace s #"!." ""))
(defn- remove-garbage [s] (str/replace s #"<[^>]*>,?" ""))
(defn score
  ([coll] (score coll 1))
  ([coll level]
   (reduce + level (map #(score % (inc level)) coll))))

(-> stream
    (scrub-bangs)
    (remove-garbage)
    (str/replace #"\{" "(")
    (str/replace #"\}" ")")
    (read-string)
    (score))
