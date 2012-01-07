(ns okasaki.streams2-tests
    (:use okasaki.streams2
          clojure.test))

(defn nats
    ([]  (nats 0))
    ([n] (->Cons n (nats (inc n)))))

(deftest first-element-stream-of-length-one
    (is (= 0 (s-first (->Cons 0 Nil)))))

(deftest first-nat-is-zero
    (is (zero? (s-first (nats)))))

(deftest we-can-get-the-nth-nat
    (let [nth #(s-first (s-drop % (nats)))]
        (is (= 10 (nth 10)))))

