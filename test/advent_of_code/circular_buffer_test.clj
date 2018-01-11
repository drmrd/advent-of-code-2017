(ns advent-of-code.circular-buffer-test
  (:require [advent-of-code.circular-buffer :refer :all])
  (:require [clojure.test :refer :all])
  (:import [advent_of_code.circular_buffer CircularBuffer]))


(deftest about-the-length-of-a-CircularBuffer
  (testing "Empty buffers contain no elements"
    (is (= 0 (count (CircularBuffer. [] 0)))))

  (testing "The start of the buffer doesn't affect its size"
    (is (= 5 (count (CircularBuffer. (range 5) 0))))
    (is (= 5 (count (CircularBuffer. (range 5) 2))))
    (is (= 5 (count (CircularBuffer. (range 5) 4))))))


(deftest about-CircularBuffer-indices
  (testing "Indices are actually 'circular'"
    (is (= (nth (CircularBuffer. (range 5) 0) 0)
           (nth (CircularBuffer. (range 5) 0) 5)))

    (is (= (nth (CircularBuffer. (range 5) 0) 2)
           (nth (CircularBuffer. (range 5) 0) 7)))

    (is (= (nth (CircularBuffer. (range 5) 0) 4)
           (nth (CircularBuffer. (range 5) 0) 9)))

    (is (= (nth (CircularBuffer. (range 5) 5) 0)
           (nth (CircularBuffer. (range 5) 0) 0)))))


(deftest about-CircularBuffers-equivalence
  (let [r5 (range 5)
        cb5-at-2-as-a-seq '(2 3 4 0 1)
        cb5 (CircularBuffer. r5 0)
        cb5-at-2 (CircularBuffer. r5 2)]
    (testing "Circular buffers are equivalent to their seq forms"
      (is (.equiv cb5 r5))
      (is (.equiv cb5-at-2 cb5-at-2-as-a-seq)))
    (testing "… but not equal to them"
      (is (not (.equals cb5 r5)))
      (is (not (.equals cb5-at-2 cb5-at-2-as-a-seq))))))


(deftest about-CircularBuffer-subsequences
  (let [cb5 (CircularBuffer. (range 5) 0)
        cb5-at-2 (CircularBuffer. (range 5) 2)]

    (testing "Take & drop begin at the current starting index"
      (is (= 0 (first (take 1 cb5))))
      (is (= 2 (first (take 1 cb5-at-2))))

      (is (= 1 (first (drop 1 cb5))))
      (is (= 3 (first (drop 1 cb5-at-2)))))

    (testing "Take & drop respect a buffer's circular structure"
      (is (= '(2 3 4 0) (take 4 cb5-at-2)))
      (is (= '(1) (take 1 (drop 4 cb5-at-2)))))))


(deftest about-replacing-circular-subsequences
  (let [cb5 (CircularBuffer. (range 5) 0)
        cb5-at-2 (CircularBuffer. (range 5) 2)]

    (testing "Contiguous subsequences in the underlying collection can be replaced"
      (is (= '(:a :b :c 3 4)
             (seq (replace-subseq cb5 [:a :b :c])))))

    (testing "Non-contiguous subs are also correctly replaced"
      (is (= '(:a :b :c :d 1)
             (seq (replace-subseq cb5-at-2 [:a :b :c :d])))))

    (testing "The starting point of a subsequence can be changed"
      (is (= '(0 :a :b :c 4)
             (seq (replace-subseq cb5 [:a :b :c] 1)))))

    (testing "The the start of the replacement CircularBuffer can be shifted"
      (is (= '(:b :c 3 4 :a)
             (seq (replace-subseq cb5 [:a :b :c] 0 1)))))))


(deftest about-reversing-circular-subsequences
  (let [cb5 (->CircularBuffer (range 5) 0)]
    (testing "Reversing an empty subsequence does nothing"
      (is (= cb5 (reverse-subseq cb5 0 0 0))))

    (testing "Reversing a length 1 subsequence does nothing"
      (is (= cb5 (reverse-subseq cb5 0 1 0))))

    (testing "The subsequence need not be taken from the beginning"
      (is (= (->CircularBuffer [0 3 2 1 4] 0)
             (reverse-subseq cb5 1 3 0))))

    (testing "The buffer's start may be shifted after a reversal"
      (is (= (->CircularBuffer [2 1 4 0 3] 0)
             (reverse-subseq cb5 1 3 2))))

    (testing "The subsequence that's reversed is circular"
      (is (= (->CircularBuffer [3 1 2 0 4] 0)
             (reverse-subseq cb5 3 3 0))))))
