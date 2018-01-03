(ns advent-of-code.day8
  (:require [advent-of-code.core :refer [file-lines]]
            [clojure.string :as str]
            [criterium.core :as criterium]))

(def instructions (file-lines "resources/day8_input.txt"))

(def operations {"inc" +, "dec" -, ">" >, "<" <, ">=" >=, "<=", <=,
                 "==" ==, "!=" not=})

(defn tokenize [instruction]
  (as-> (str/split instruction #" if ") tokens
    (map #(str/split % #" ") tokens)
    (map (fn [[r op v]] [(operations op) r (read-string v)]) tokens)))

(defn execute-instructions [instructions]
  (loop [instructions (map tokenize instructions)
         registers {}
         max-ever Integer/MIN_VALUE]
    (if (empty? instructions)
      {:cur-max (apply max (vals registers)) :max-ever max-ever}
      (let [[[op1 r1 v1][op2 r2 v2]] (first instructions)
            predicate (op2 (get registers r2 0) v2)
            cur-val (get registers r1 0)
            new-val (if predicate (op1 cur-val v1) cur-val)]
        (recur (rest instructions)
               (assoc registers r1 new-val)
               (max max-ever new-val))))))

(execute-instructions instructions)     ; => {:cur-max 6012, :max-ever 6369}

(comment (criterium/with-progress-reporting
           (criterium/bench (execute-instructions instructions))))

;; Evaluation count : 10980 in 60 samples of 183 calls.
;; Execution time mean : 5.561989 ms
;; Execution time std-deviation : 69.809859 µs
;; Execution time lower quantile : 5.446434 ms ( 2.5%)
;; Execution time upper quantile : 5.670576 ms (97.5%)
;; Overhead used : 9.039714 ns
