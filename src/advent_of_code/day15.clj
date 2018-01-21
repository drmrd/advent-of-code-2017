(ns advent-of-code.day15)

(defn- geometric-int32s [start ratio]
  (lazy-seq
   (cons start (geometric-int32s (mod (* start ratio) Integer/MAX_VALUE) ratio))))

(defn- multiples-of [factor] (filter #(zero? (mod % factor))))
(defn- take-last-16-bits-of-multiples [total factor] (comp (multiples-of factor)
                                                        (map #(mod % 65536))
                                                        (take total)))

(defn- =-indicator
  ([] ())
  ([& colls]
   (lazy-seq
    (let [ss (map seq colls)]
      (when (every? identity ss)
        (cons (if (apply = (map first ss)) 1 0)
              (apply =-indicator (map rest ss))))))))

(defn- count-=-xforms [xf1 s1 xf2 s2]
  (reduce + (=-indicator (eduction xf1 s1) (eduction xf2 s2))))

(count-=-xforms (take-last-16-bits-of-multiples 40000000 1)
                (geometric-int32s 289 16807)
                (take-last-16-bits-of-multiples 40000000 1)
                (geometric-int32s 629 48271))

(count-=-xforms (take-last-16-bits-of-multiples 5000000 4)
                (geometric-int32s 289 16807)
                (take-last-16-bits-of-multiples 5000000 8)
                (geometric-int32s 629 48271))
