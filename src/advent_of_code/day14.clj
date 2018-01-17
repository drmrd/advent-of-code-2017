(ns advent-of-code.day14
  (:require [advent-of-code.day10 :refer [knot-hash]]
            [clojure.pprint :refer [cl-format]]
            [criterium.core :as criterium]))

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

(reduce + (flatten (binary-hashes key-string))) ; => 8226 (Part 1 Solution)
