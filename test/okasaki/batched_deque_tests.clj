(ns okasaki.batched-deque-tests
    (:refer-clojure :exclude [cons empty? last split-at])
    (:use okasaki.batched-deque
          okasaki.list
          clojure.test))

(deftest we-can-tests-for-emptyness
    (is (empty? Empty))
    (is (not (empty? (cons 1 Empty))))
    (is (not (empty? (snoc 1 Empty)))))

(deftest empty-throws-exception
    (is (thrown? IllegalStateException (head Empty)))
    (is (thrown? IllegalStateException (tail Empty)))
    (is (thrown? IllegalStateException (last Empty)))
    (is (thrown? IllegalStateException (init Empty))))

(deftest one-element-queue
    (let [rear1 (snoc 1 Empty)
          front1 (cons 1 Empty)]
        (is (= 1 (head rear1)))
        (is (= 1 (head front1)))
        (is (= 1 (last rear1)))
        (is (= 1 (last front1)))
        (is (empty? (tail rear1)))
        (is (empty? (init rear1)))
        (is (empty? (tail front1)))
        (is (empty? (init front1)))))

(deftest two-element-queue
    (let [rear2 (snoc 2 (snoc 1 Empty))
          front2 (cons 1 (cons 2 Empty))]
        (is (= 1 (head rear2)))
        (is (= 1 (head front2)))
        (is (= 2 (last rear2)))
        (is (= 2 (last front2)))
        (is (= 2 (head (tail rear2))))
        (is (= 2 (head (tail front2))))
        (is (= 2 (last (tail rear2))))
        (is (= 2 (last (tail front2))))
        (is (= 1 (last (init rear2))))
        (is (= 1 (last (init front2))))
        (is (= 1 (head (init rear2))))
        (is (= 1 (head (init front2))))))

(deftest many-elements-queue
    (let [n 25
          elems (range n)
          rear (reduce #(snoc %2 %1) Empty elems)
          front (reduce #(cons %2 %1) Empty elems)
          sym (fn [i] (- n (+ i 1)))
          nth-first (fn [n q] (head (first (drop n (iterate tail q)))))
          nth-last (fn [n q] (last (first (drop n (iterate init q)))))]
        (doseq [i elems]
            (is (= i (nth-first i rear)))
            (is (= (sym i) (nth-last i rear)))
            (is (= (sym i) (nth-first i front)))
            (is (= i (nth-last i front))))))
