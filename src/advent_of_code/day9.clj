(ns advent-of-code.day9
  (:require [clojure.string :as str]))

(def stream (slurp "resources/day9_input.txt"))

(defn- scrub-bangs [s] (str/replace s #"!." ""))
(defn- collect-garbage [s] (re-seq #"<([^>]*)>,?" s))
(defn- remove-garbage [s] (str/replace s #"<[^>]*>,?" ""))
(defn score
  ([coll] (score coll 1))
  ([coll level]
   (reduce + level (map #(score % (inc level)) coll))))

(let [cleaner-stream (scrub-bangs stream)
      clean-stream (remove-garbage cleaner-stream)
      garbage (collect-garbage cleaner-stream)]

  {:part1-solution (-> clean-stream
                       (str/replace #"\{" "(")
                       (str/replace #"\}" ")")
                       (read-string)
                       (score))
   :part2-solution (->> garbage
                        (map (comp count second))
                        (reduce +))})
