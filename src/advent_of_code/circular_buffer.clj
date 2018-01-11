(ns advent-of-code.circular-buffer
  (:import (clojure.lang Counted
                         Indexed
                         IPersistentCollection
                         ISeq
                         PersistentVector))
  (:require [clojure.set :as set]))


(defprotocol CircularlySubsequenced
  (replace-subseq
    [this new-subseq]
    [this new-subseq sub-start]
    [this new-subseq sub-start new-start-shift])
  (reverse-subseq [this sub-start sub-count new-start-shift]))


(declare count-or-self circular-index)


(deftype CircularBuffer [^ISeq coll ^int start]
  :load-ns true

  Object
  (equals [this that]
    (and (= (type this) (type that))
         (= (seq this) (seq that))))
  (toString [this]
    (let [v (vec coll)
          s (if-not (empty? v) (assoc v start [(nth v start)]) v)]
      (str (seq s))))

  Counted
  (count [_] (count coll))

  Indexed
  (nth [this ^int i not-found] (nth coll (circular-index i start this) not-found))
  (nth [this ^int i] (nth this i nil))

  IPersistentCollection
  (seq [this] (concat (drop start coll) (take start coll)))
  (equiv [this that] (and (= (seq this) (seq that))))

  CircularlySubsequenced
  (replace-subseq [this new-subseq sub-start new-start-shift]
    (if (> (count new-subseq) (count this))
      (throw (IllegalArgumentException.
              "The replacement subsequence length is larger than the buffer"))
      (let [cb-count (count this)
            cb-start (circular-index 0 start cb-count)

            sub-count (count new-subseq)
            sub-start (circular-index sub-start cb-start cb-count)

            tail-sub-count (min sub-count (- cb-count sub-start))
            head-sub-count (max 0 (- sub-count tail-sub-count))

            first-vals (zipmap (range 0 head-sub-count)
                               (take-last head-sub-count new-subseq))

            last-vals (zipmap (range sub-start
                                     (+ sub-start tail-sub-count))
                              (take tail-sub-count new-subseq))

            indices->new-vals (merge first-vals last-vals)]

        (CircularBuffer. (reduce-kv #(assoc %1 %2 %3) coll indices->new-vals)
                         (circular-index (+ cb-start new-start-shift)
                                         0 cb-count)))))

  (replace-subseq [this new-subseq sub-start]
    (replace-subseq this new-subseq sub-start 0))

  (replace-subseq [this new-subseq]
    (replace-subseq this new-subseq 0 0))

  (reverse-subseq [this sub-start sub-count new-start-shift]
    (let [shifted-cb (-> this (seq) (CircularBuffer. sub-start))
          reversed-sub (->> shifted-cb (take sub-count) (reverse))]
      (replace-subseq this reversed-sub sub-start new-start-shift))))


(defn- count-or-self [countable]
  (condp instance? countable
    Counted (count countable)
    Number countable
    (throw (IllegalArgumentException.
            "Expected something counted or a number."))))


(defn- circular-index [i start countable]
  (mod (+ start i) (count-or-self countable)))
