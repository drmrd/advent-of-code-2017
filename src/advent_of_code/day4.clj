(ns advent-of-code.day4
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(use 'clojure.java.io)

(defn- all-distinct? [words] (= (distinct words) words))
(defn no-anagrams? [words] (->> words (map (comp sort seq)) all-distinct?))

(defn- count-valid-passphrases [words-test]
  (with-open [rdr (reader "resources/day4_input.txt")]
    (->> rdr
         (line-seq)
         (map (fn [line]
                (let [words (str/split line (re-pattern " +"))]
                  (if (words-test words) 1 0))))
         (reduce +))))

(count-valid-passphrases all-distinct?)
(count-valid-passphrases #(and (all-distinct? %) (no-anagrams? %)))
