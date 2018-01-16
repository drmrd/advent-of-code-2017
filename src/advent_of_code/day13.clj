(ns advent-of-code.day13
  (:require [clojure.string :as str]))

(def firewall-layers (->> "resources/day13_input.txt"
                          (slurp)
                          (#(str/split % #": |\n"))
                          (map read-string)
                          (partition 2)
                          (map (partial zipmap [:depth :range]))))

(defn- scanner-period [layer] (-> layer (:range) (dec) (* 2)))

(defn- scanner-position [layer elapsed-time]
  (or (->> layer (scanner-period) (mod elapsed-time)) 0))

(defn- scan-severity [layers]
  (->> layers
       (filter (fn [l] (zero? (scanner-position l (:depth l)))))
       (map #(reduce * (vals %)))
       (reduce +)))

(scan-severity firewall-layers)         ; => 1580 (Part 1 Solution)
