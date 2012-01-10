(ns okasaki.binomial-heap-tests
    (:refer-clojure :exclude [merge split-at])
    (:use okasaki.list
          okasaki.binomial-heap
          clojure.test))

(deftest minimum-one-elememt
    (is (= 1 (findMin (insert 1 Nil)))))

(deftest minimum-two-elements
    (is (= 1 (findMin (insert 1 (insert 2 Nil)))))
    (is (= 1 (findMin (insert 2 (insert 1 Nil))))))

(deftest minimun-more-elements
    (let [elems [6 3 5 8 4 1 2 7]
          heap (reduce #(insert %2 %1) Nil elems)]
        (is (= 1 (findMin heap)))))

(deftest minimun-after-deletemin
    (let [elems [6 3 5 8 4 1 2 7]
          heap (reduce #(insert %2 %1) Nil elems)
          heap (deleteMin heap)]
        (is (= 2 (findMin heap)))))
