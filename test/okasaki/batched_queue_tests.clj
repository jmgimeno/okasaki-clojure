(ns okasaki.batched-queue-tests
    (:refer-clojure :exclude [empty?])
    (:use okasaki.batched-queue
          clojure.test))

(deftest empty-throws-exception
    (is (thrown? IllegalStateException (head Empty)))
    (is (thrown? IllegalStateException (tail Empty))))

(deftest one-element-queue
    (let [one (snoc Empty 1)]
        (is (= 1 (head one)))
        (is (empty? (tail one)))))

(deftest various-elements-queue
    (let [queue (reduce snoc Empty [1 2 3 4 5])]
        (is (= 1 (head queue)))
        (is (= 2 (head (tail queue))))))