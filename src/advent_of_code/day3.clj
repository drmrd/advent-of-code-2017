(ns advent-of-code.day3
  (:require [clojure.math.combinatorics :as combo]))

(defn- greatest-odd-square-at-most [x]
  (let [fx (int (Math/floor (Math/sqrt x)))]
    (if (even? fx) (dec fx) fx)))

(def side-length-complete-square greatest-odd-square-at-most)

(defn- radial-seminorm [x]
  "Returns the taxicab radial distance from X to the origin."
  (let [complete-rung-side (side-length-complete-square x)
        complete-square (* complete-rung-side complete-rung-side)
        complete-rung-count (quot complete-rung-side 2)]
    (if (= x complete-square) complete-rung-count (inc complete-rung-count))))

(defn- perfect-square? [x]
  "Returns whether or not 'x' is a perfect square."
  (-> x
      (Math/sqrt)
      (#(>= (int %) %))))

(defn- lateral-coordinate [n]
  "Returns the distance between N and the center of the line N is on."
  (cond
    (<= n 9) (if (= n 1) 0 (mod n 2))
    (and (odd? n) (perfect-square? n)) (quot (int (Math/sqrt n)) 2)
    (> n 9) (let [side-length-complete-square (side-length-complete-square n)
                  side-length-current-square (inc side-length-complete-square)
                  complete-square (* side-length-complete-square
                                     side-length-complete-square)]
              (-> n
                  (- complete-square)
                  (mod side-length-current-square)
                  (- (quot side-length-current-square 2))))))

(def lateral-seminorm (comp #(Math/abs %) lateral-coordinate))

(defn- absmax-kv [coords]
  (->> coords (apply max-key (comp #(Math/abs %) val))))

(defn taxicab-norm [x]
  (cond
    (integer? x) (+ (lateral-seminorm x) (radial-seminorm x))
    (map? x) (->> x (vals) (map #(Math/abs %)) (reduce +))))

(defn taxicab-metric [x1 x2]
  (cond
    (and (integer? x1) (integer? x2) (pos? x1) (pos? x2)) (taxicab-norm (- x1 x2))
    (and (map? x1) (map? x2)) (taxicab-norm (merge-with - x1 x2))))

(defn- first-half-of-rung? [p]
  "Returns point p occurs on the first half of its spiral rung."
  (pos? (val (absmax-kv p))))

(def second-half-of-rung? (comp not first-half-of-rung?))

(defn- at-full-rung? [{:keys [x y] :as p}]
  (and (pos? x) (= x (- y))))

(defn- at-half-rung? [{:keys [x y] :as p}]
  (and (neg? x) (= x (- y))))

(defn- next-main-diagonal-corner [{:keys [x y] :as p}]
  "Returns the next main diagonal corner in the spiral after p."
  (let [absmax-val (Math/abs (val (absmax-kv p)))
        next-corner-x (cond
                        (= p {:x 0 :y 0}) -1
                        (at-full-rung? p) (inc x)
                        (at-half-rung? p) y
                        (first-half-of-rung? p) (- absmax-val)
                        (second-half-of-rung? p) absmax-val)]
    (#(assoc {} :x % :y (- %)) next-corner-x)))

(defn spiral-norm [x]
  (cond
    (integer? x) x
    (map? x)
    (let [rung (Math/abs (val (absmax-kv x))) ; Note: 0-indexed
          rungth-odd (inc (* 2 rung))
          completed-square (#(* % %) (- rungth-odd 2))]
      ;; TODO: Use explicit formulas here:
      (if (first-half-of-rung? x)
          (+ completed-square
             (taxicab-metric {:x rung :y (- rung)} x))
          (+ completed-square
             (taxicab-metric {:x rung :y (- rung)} {:x (- rung) :y rung})
             (taxicab-metric {:x (- rung) :y rung} x))))))

(defn- find-side [n]
  "Returns the side of the spiral on which the n-th square first appears.
   A corner square is always assigned the 'second' side on which it would appear
   from the perspective of a counterclockwise spiral generation algorithm. For
   instance, a top-right corner will always return :top, since it is the bridge
   from a right side to a top side as the spiral is formed."
  (let [side-length-complete-square (side-length-complete-square n)
        side-length-current-square (+ side-length-complete-square 2)
        complete-square (* side-length-complete-square
                           side-length-complete-square)
        outer-squares (- n complete-square)
        side-order (quot outer-squares (dec side-length-current-square))]
    (case side-order 0 :right 1 :top 2 :left 3 :bottom)))

(defn- at-main-diag-corner? [n]
  (let [side-length-complete-square (side-length-complete-square n)
        complete-square (* side-length-complete-square
                           side-length-complete-square)
        next-main-corner (-> side-length-complete-square
                             (inc)
                             (#(* % %))
                             (inc))]
    (or (= n complete-square)
        (= n next-main-corner))))

(defn- other-key [key]
  (->> [:x :y]
       (filter #(not= % key))
       (first)))

(defn- signum [n]
  "Returns the signum of an integer n."
  {:pre [(integer? n)]}
  (compare n 0))

(defn- spiral-position->coordinates [n]
  (let [side->radial-data {:left [:x -1] :right [:x 1]
                           :bottom [:y -1] :top [:y 1]}
        [radial-coord-symb radial-coord-sign] (-> n
                                                  (find-side)
                                                  (side->radial-data))
        lateral-coord-symb (other-key radial-coord-symb)]
    (-> {}
        (assoc radial-coord-symb (* (radial-seminorm n) radial-coord-sign))
        (assoc lateral-coord-symb (* (lateral-coordinate n)
                                     (if (at-main-diag-corner? n) -1 1))))))

(defn- adjacent-earlier-positions [coords]
  "Return the indices of all of the positions in the memory spiral that are less
   than and adjacent to the square at 'coords'."
  (let [norm (spiral-norm coords)]
    (filter (fn [p] (< (spiral-norm p) norm))
            (map (fn [[x y]] (merge-with + coords {:x x :y y}))
                 (combo/cartesian-product [-1 0 1] [-1 0 1])))))

(defn spiralnacci [coords]
  "Returns the value of the n-th spiralnacci number."
  (if (= (spiral-norm coords) 1) 1
      (reduce + (map spiralnacci (adjacent-earlier-positions coords)))))

(defn memoize [f]
  (let [mem (atom {})]
    (fn [& args]
      (println @mem)
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(def memoized-spiralnacci (memoize spiralnacci))

(taxicab-norm 325489)
(map memoized-spiralnacci
     [{:x 0 :y 0} {:x 1 :y 0} {:x 1 :y 1} {:x 0 :y 1} {:x -1 :y 1}
      {:x -1 :y 0} {:x -1 :y -1} {:x 0 :y -1} {:x 1 :y -1}
      {:x 2 :y -1} {:x 2 :y 0} {:x 2 :y 1} {:x 2 :y 2} {:x 1 :y 2}
      {:x 0 :y 2} {:x -1 :y 2} {:x -2 :y 2} {:x -2 :y 1}
      {:x -2 :y 0} {:x -2 :y -1} {:x -2 :y -2} {:x -1 :y -2}
      {:x 0 :y -2} {:x 1 :y -2} {:x 2 :y -2} {:x 3 :y -2}
      {:x 3 :y -1} {:x 3 :y 0} {:x 3 :y 1} {:x 3 :y 2} {:x 3 :y 3}
      {:x 2 :y 3} {:x 1 :y 3} {:x 0 :y 3} {:x -1 :y 3} {:x -2 :y 3}
      {:x -3 :y 2} {:x -3 :y 1} {:x -3 :y 0} {:x -3 :y -1}
      {:x -3 :y -2} {:x -3 :y -3} {:x -2 :y -3} {:x -1 :y -3}
      {:x 0 :y -3} {:x 1 :y -3} {:x 2 :y -3} {:x 3 :y -3}
      {:x 4 :y -3} {:x 4 :y -2} {:x 4 :y -1} {:x 4 :y 0} {:x 4 :y 1}
      {:x 4 :y 2} {:x 4 :y 3} {:x 4 :y 4} {:x 3 :y 4} {:x 2 :y 4}
      {:x 1 :y 4} {:x 0 :y 4} {:x -1 :y 4}])
