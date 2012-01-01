(ns okasaki.list-tests
    (:use okasaki.list
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