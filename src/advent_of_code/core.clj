(ns advent-of-code.core)

(defn int-to-digit-list [n]
  (loop [rem n result (transient '())]
    (if (= rem 0)
      (if (empty? result) '(0) result)
      (recur (quot rem 10)
             (-> rem
                 (mod 10)
                 (list)
                 (concat result))))))
