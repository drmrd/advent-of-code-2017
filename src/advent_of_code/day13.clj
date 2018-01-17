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

(defn- scan-severity [layers start-time]
  (->> layers
       (filter (fn [l] (zero? (scanner-position l (+ (:depth l) start-time)))))
       (map #(reduce * (vals %)))
       (reduce +)))

(scan-severity firewall-layers 0)       ; => 1580 (Part 1 Solution)

(defn- gcd [m n] (if (zero? n) m (recur n (mod m n))))
(defn- lcm [m n] (/ (* m n) (gcd m n)))

(defn- scanner-periods [layers]
  (->> layers (map scanner-period) (distinct)))

(defn- lcm-of-scanner-periods [layers]
  (->> layers (map scanner-period) (distinct) (reduce lcm)))

(defn- delay-to-avoid-detection [layers]
  (let [undetected-in-layer?
        (fn [delay layer]
          (-> delay (+ (:depth layer)) (mod (scanner-period layer)) (pos?)))
        scot-free?
        (fn [delay]
          (reduce #(and %1 %2)
                  (map (partial undetected-in-layer? delay) layers)))
        max-delay (lcm-of-scanner-periods layers)]
    (->> max-delay (range) (keep-indexed #(when (scot-free? %2) %1)) (first))))

(delay-to-avoid-detection firewall-layers) ; => 3943252 (Part 2 Solution)
