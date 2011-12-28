(ns okasaki.leftist-heap-tests
    (:refer-clojure :exclude [merge])
    (:use okasaki.leftist-heap
          clojure.test))

(deftest minimum-of-a-single-node-heap
    (is (= 1 (findMin (insert 1 Empty)))))

(deftest minimum-of-a-two-nodes-heap
    (is (= 1 (findMin (insert 1 (insert 2 Empty)))))
    (is (= 1 (findMin (insert 2 (insert 1 Empty))))))

(deftest second-minimum-after-removing-minimum
    (is (= 2 (findMin (deleteMin (insert 1 (insert 2 Empty))))))
    (is (= 2 (findMin (deleteMin (insert 2 (insert 1 Empty)))))))
