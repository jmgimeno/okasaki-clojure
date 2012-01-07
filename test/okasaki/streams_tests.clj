(ns okasaki.streams-tests
    (:use okasaki.streams
          datatype.core
          clojure.test))

(defn nats
    ([]  (nats 0))
    ([n] ($ (->Cons n (nats (inc n))))))

(deftest first-element-stream-of-length-one
    (is (= 0 (s-first ($ (->Cons 0 Nil))))))

(deftest first-nat-is-zero
    (is (zero? (s-first (nats)))))

(deftest we-can-get-the-nth-nat
    (let [nth #(s-first (s-drop % (nats)))]
        (is (= 10 (nth 10)))))

(defn- counted [counter value]
    (swap! counter inc)
    value)

(deftest we-can-append-lazyly
    (let [num-evals (atom 0)
          nil1  ($ (counted num-evals Nil))
          str1  ($ (counted num-evals (->Cons 3 nil1)))
          str1  ($ (counted num-evals (->Cons 2 str1)))
          str1  ($ (counted num-evals (->Cons 1 str1)))
          nil2  ($ (counted num-evals Nil))
          str2  ($ (counted num-evals (->Cons 5 nil2)))
          str2  ($ (counted num-evals (->Cons 4 str2)))
          res   (s-append str1 str2)]
        (is (= 0 @num-evals))
        (is (= 1 (s-first res)))
        (is (= 1 @num-evals))
        (is (= 2 (s-first (s-rest res))))
        (is (= 2 @num-evals))
        (is (= 3 (s-first (s-rest (s-rest res)))))
        (is (= 3 @num-evals))
        (is (= 4 (s-first (s-rest (s-rest (s-rest res))))))
        (is (= 5 @num-evals))))

(deftest we-can-take-lazyly
    (let [num-evals (atom 0)
          str (reduce #($ (counted num-evals (->Cons %2 %1))) ($ (counted num-evals Nil)) [5 4 3 2 1])]
        (is (= 0 @num-evals))
        (is (= 1 (s-first (s-take 3 str))))
        (is (= 1 @num-evals))
        (is (= 2 (s-first (s-rest (s-take 3 str)))))
        (is (= 2 @num-evals))))

(deftest we-can-take-lazyly
    (let [num-evals (atom 0)
          str (reduce #($ (counted num-evals (->Cons %2 %1))) ($ (counted num-evals Nil)) [5 4 3 2 1])]
        (is (= 0 @num-evals))
        (is (= 1 (s-first (s-drop 0 str))))
        (is (= 1 @num-evals))
        (s-drop 3 str)
        (is (= 1 @num-evals))
        (is (= 4 (s-first (s-drop 3 str))))
        (is (= 4 @num-evals))))

(deftest we-can-reverse-a-stream
    (let [num-evals (atom 0)
          str (reduce #($ (counted num-evals (->Cons %2 %1))) ($ (counted num-evals Nil)) [5 4 3 2 1])]
        (is (= 0 @num-evals))
        (is (= 5 (s-first (s-reverse str))))
        (is (= 6 @num-evals))))
    
