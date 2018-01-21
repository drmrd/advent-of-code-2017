(ns advent-of-code.day16
  (:require [clojure.string :as str]))

(def dance-moves (-> "resources/day16_input.txt" (slurp) (str/trim-newline) (str/split #",")))

(defn- spin [n]
  (fn [programs]
    (vec (concat (take-last n programs) (drop-last n programs)))))

(defn- exchange [a b]
  (fn [programs]
    (let [pa (nth programs a) pb (nth programs b)]
      (-> programs (assoc a pb) (assoc b pa)))))

(defn- partner [pa pb]
  (fn [programs]
    (let [a (.indexOf programs pa) b (.indexOf programs pb)]
      ((exchange a b) programs))))

(defn- parse-move [record]
  (let [move (first record) details (apply str (rest record))]
    (case move
      \s ((comp spin #(Integer/parseInt %)) details)
      \x (apply exchange (map #(Integer/parseInt %) (str/split details #"/")))
      \p (apply partner (map keyword (str/split details #"/"))))))

(defn- just-dance [move-records]
  (fn [programs]
    (let [dance (apply comp (reduce conj '() (map parse-move move-records)))]
      (dance programs))))

(def part1-solution (->> [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p]
                         ((just-dance dance-moves))
                         (map name)
                         (apply str)))  ; => olgejankfhbmpidc
