(ns advent-of-code.day12
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(def connected-programs
  (->> "resources/day12_input.txt"
    (slurp)
    (str/split-lines)
    (map #(str/split % #"\s*<->\s*"))
    (flatten)
    (vec)
    (apply hash-map)
    (walk/keywordize-keys)
    (map (fn [[k v]]
           (let [nbs (map keyword (str/split v #", "))]
             (map #(vector k %) nbs))))
    (flatten)
    (partition 2)
    (map vec)
    (apply uber/graph)
    (alg/connected-components)))

(defn connected-component [ccs node]
  (->> ccs
       (filter #(some #{:0} %))
       ((fn [[connected-nodes]] (count (distinct connected-nodes))))))

(defn connected-components [ccs] (count ccs))

(connected-component connected-programs :0)
(connected-components connected-programs)
