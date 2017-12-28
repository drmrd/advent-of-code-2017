(ns advent-of-code.day7-test
  (:require [advent-of-code.day7 :refer :all]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(deftest test-parse-tower-notes
  (testing "The note parser correctly handles nodes with children"
    (is (= (parse-tower-notes "foo (42) -> bar, baz")
           (list (->Program "foo" 42 ["bar" "baz"])))))
  (testing "The note parser correctly processes leaves"
    (is (= (parse-tower-notes "foo (42)")
           (list (->Program "foo" 42 []))))))

(deftest test-bottom-program
  (testing "The right root program is found in the sample tower"
    (let [sample-programs (->> ["pbga (66)" "xhth (57)" "ebii (61)"
                                "havc (66)" "ktlj (57)" "qoyq (66)"
                                "padx (45) -> pbga, havc, qoyq"
                                "tknk (41) -> ugml, padx, fwft" "jptl (61)"
                                "ugml (68) -> gyxo, ebii, jptl"
                                "fwft (72) -> ktlj, cntj, xhth" "gyxo (61)"
                                "cntj (57)"]
                               (str/join "\n")
                               (parse-tower-notes))]
      (is (= (:name (bottom-program sample-programs)) "tknk")))))
