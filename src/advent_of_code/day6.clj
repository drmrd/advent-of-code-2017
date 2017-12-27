(ns advent-of-code.day6
  (:require [advent-of-code.core :refer [file->vec]])
  (:import [java.util ArrayList Set LinkedHashSet]))

(set! *warn-on-reflection* true)

(def memory-array (file->vec "resources/day6_input.txt"))

(defn- max-info [s]
  "Return the index and value of the maximum member of a sequence."
  (let [max-val (apply max s)]
    {:max-idx (.indexOf s max-val) :max-val max-val}))

(defn- new-blocks-at-bank [{:keys [cur-idx max-idx max-val
                                   blocks-for-each blocks-leftover
                                   leftovers-pre-max leftovers-post-max]}]
  (let [gets-a-leftover-block? (or (< cur-idx leftovers-pre-max)
                                   (and (> cur-idx max-idx)
                                        (< cur-idx (+ max-idx leftovers-post-max 1))))]
    (+ blocks-for-each (if gets-a-leftover-block? 1 0))))

;; TODO: Handle the variable verbosity here
(defn reallocate [memory]
  (let [{:keys [max-idx max-val]} (max-info memory)
        bank-count (count memory)
        blocks-for-each (quot max-val bank-count)
        blocks-leftover (mod max-val bank-count)
        leftovers-post-max (min blocks-leftover (- (dec bank-count) max-idx))
        leftovers-pre-max (max (- blocks-leftover leftovers-post-max) 0)
        new-banks (transient [])]
    (dorun (for [i (range bank-count) :let [cur-val (nth memory i)]]
             (conj! new-banks
                    (+ (if (not= max-idx i) cur-val 0)
                       (new-blocks-at-bank {:cur-idx i :max-idx max-idx :max-val max-val
                                            :blocks-for-each blocks-for-each
                                            :blocks-leftover blocks-leftover
                                            :leftovers-pre-max leftovers-pre-max
                                            :leftovers-post-max leftovers-post-max})))))
    (persistent! new-banks)))

(defn- element? [^Set set element]
  "Determines whether a given java.util.Set contains an element."
  (.contains set element))

(defn reallocation-cycle-data [initial-memory-banks]
  (let [history (LinkedHashSet.)
        bank-count (count initial-memory-banks)]
    (loop [cycles 0 memory initial-memory-banks]
      (if (element? history memory)
        {:cycle-count cycles
         :loop-length (- (count history) (.indexOf (ArrayList. history) memory))}
        (do (.add history memory)
            (recur (inc cycles) (reallocate memory)))))))

(reallocation-cycle-data memory-array)
