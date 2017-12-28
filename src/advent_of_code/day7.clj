(ns advent-of-code.day7
  (:require [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(set! *warn-on-reflection* true)

(defn- note->graph-inits [note]
  (let [[name weight children] (-> note
                                   (str/replace #"\(|\)" "")
                                   (str/split #"\s+(:?->)?\s*" 3))
        name-keyword (keyword name)
        program-init [name-keyword {:weight (read-string weight)}]]
     (if-not (empty? children)
       (conj (->> children
                  (#(str/split % #", "))
                  (map keyword)
                  (map #(vector name-keyword %)))
             program-init)
       [program-init])))

(defn program-tree [notes]
  (->> notes
       (str/split-lines)
       (map note->graph-inits)
       (reduce concat)
       (apply uber/digraph)))

(def PT (->> "resources/day7_input.txt" (slurp) (program-tree)))

(defn bottom-program [PT]
  (first (filter (comp zero? (partial uber/in-degree PT)) (uber/nodes PT))))

(defn- tower-weight-at [PT node]
  (+ (uber/attr PT node :weight)
     (reduce + (map (partial tower-weight-at PT) (uber/successors PT node)))))

(defn- tower-weights-of-children [PT node]
  (zipmap (uber/successors PT node)
          (map (partial tower-weight-at PT) (uber/successors PT node))))

(defn- unbalanced-child [node]
  (->> node
       (tower-weights-of-children PT)
       (group-by val)
       (map (juxt key (comp (partial map key) val)))
       (clojure.set/map-invert)
       (filter (fn [[keys weight]] (= (count keys) 1)))
       (first)
       ((fn [[skey weight]] (if (nil? weight) nil [(first skey) weight])))))

(defn- highest-unbalanced-program-in-tower [node]
  (let [bc (first (unbalanced-child node))]
    (if bc (highest-unbalanced-program-in-tower bc) node)))

(def children (uber/successors PT))
(def predecessor (comp first (uber/predecessors PT)))
(def siblings (comp children predecessor))
(defn- sibling-tower-weight [node]
  (->> node
       (siblings)
       (filter #(not= node %))
       (first)
       (tower-weight-at PT)))

(defn corrected-weight [G]
  (let [node-to-change (-> G
                           bottom-program
                           highest-unbalanced-program-in-tower)
        weight-adjustment (- (sibling-tower-weight node-to-change)
                             (tower-weight-at G node-to-change))]
    (+ (uber/attr G node-to-change :weight) weight-adjustment)))

(def day7-part1-solution (bottom-program PT))
(def day7-part2-solution (corrected-weight PT))

{:part1 day7-part1-solution :part2 day7-part2-solution}
