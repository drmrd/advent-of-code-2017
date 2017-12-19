(ns advent-of-code.day1-test
  (:require [advent-of-code.day1 :refer :all]
            [clojure.test :refer :all]))

(deftest sum-of-repeat-digits-in-1-is-1-test
  (testing "There is one repeated digit in 1"
    (is (= (sum-repeats 1) 1))))

(deftest sum-of-repeat-digits-in-1122-is-3-test
  (testing "The sum of the repeated digits in 1122 is 3"
    (is (= (sum-repeats 1122) 3))))

(deftest sum-of-repeat-digits-in-1111-is-4-test
  (testing "The sum of the repeated digits in 1111 is 4"
    (is (= (sum-repeats 1111) 4))))

(deftest sum-of-repeat-digits-in-91212129-is-9-test
  (testing "The sum of the repeated digits in 91212129 is 9"
    (is (= (sum-repeats 91212129) 9))))

(deftest sum-of-shifted-repeated-digits-in-1212-is-6-test
  (testing "The sum of the shifted repeated digits in 1212 is 6"
    (is (= (sum-half-length-repeats 1212) 6))))

(deftest sum-of-shifted-repeated-digits-in-1221-is-0-test
  (testing "The sum of the shifted repeated digits in 1221 is 0"
    (is (= (sum-half-length-repeats 1221) 0))))

(deftest sum-of-shifted-repeated-digits-in-123425-is-4-test
  (testing "The sum of the shifted repeated digits in 123425 is 4"
    (is (= (sum-half-length-repeats 123425) 4))))

(deftest sum-of-shifted-repeated-digits-in-123123-is-12-test
  (testing "The sum of the shifted repeated digits in 123123 is 12"
    (is (= (sum-half-length-repeats 123123) 12))))

(deftest sum-of-shifted-repeated-digits-in-12131415-is-4-test
  (testing "The sum of the shifted repeated digits in 12131415 is 4"
    (is (= (sum-half-length-repeats 12131415) 4))))
