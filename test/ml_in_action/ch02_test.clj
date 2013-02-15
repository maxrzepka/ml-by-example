(ns ml-in-action.ch02
  (:use clojure.test
        ml-in-action.ch02))

(deftest test-classify0
  (testing "Simple classification"
    (is (= :B (classify0 data0 1 [0 0])))))
