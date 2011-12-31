(ns okasaki.binomial-heap-tests
    (:refer-clojure :exclude [merge])
    (:use okasaki.binomial-heap
          clojure.test))

(deftest list-append
    (let [l1 (Cons 1 (Cons 2 Nil))
          l2 (Cons 3 (Cons 4 Nil))
          l3 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))]
        (is (= l1 (append l1 Nil)))
        (is (= l1 (append Nil l1)))
        (is (= l3 (append l1 l2)))))

(deftest list-reverse
    (let [l (Cons 1 (Cons 2 (Cons 3 Nil)))
          r (Cons 3 (Cons 2 (Cons 1 Nil)))]
        (is (= r (rev l)))))

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
