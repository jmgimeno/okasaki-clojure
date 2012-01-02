(ns datatype.core-tests
    (:use datatype.core
          clojure.test))

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
       ~@tests))

(defdatatype ::type expr (ctor arg1 arg2))

(deftest empty-constructor-returns-keyword-in-current-namespace
    (is (= ::expr expr)))

(deftest non-empty-constructor-creates-record
    (let [elem (->ctor 'x 'y)]
        (is (= 'x (:arg1 elem)))
        (is (= 'y (:arg2 elem)))))

(deftest constructors-have-the-right-metadata
    (is (= ::type (:datatype.core/datatype (meta #'expr))))
    (is (= ::type (:datatype.core/datatype (meta #'->ctor)))))

(deftest caseof-works-properly
    (let [test #(caseof [%] [expr] 0 [[ctor x y]] (+ x y))]
	(is (= 0 (test expr)))
	(is (= 3 (test (->ctor 1 2))))))

(comment

 (deflazy ::lazy lexpr (lctor arg1 arg2))

 (defdatatype ::lazy2 ^:datatype.core/lazy lexpr2 (^:datatype.core/lazy lctor2 arg1 arg2))

 (deftest empty-constructor-returns-delayed-keyword-in-current-namespace
     (is (delay? lexpr))
     (is (= ::lexpr (force lexpr)))
     (is (delay? lexpr2))
     (is (= ::lexpr2 (force lexpr2))))

 (deftest non-empty-constructor-returns-delayed-vector
     (is (delay? (lctor 'x 'y)))
     (is (= [::lctor 'x 'y] (force (lctor 'x 'y))))
     (is (delay? (lctor2 'x 'y)))
     (is (= [::lctor2 'x 'y] (force (lctor2 'x 'y)))))

 (deftest caseof-works-properly-with-lazy
     (let [test #(caseof [%] [lexpr] 0 [[lctor x y]] (+ x y))]
         (is (= 0 (test lexpr)))
         (is (= 3 (test (lctor 1 2))))))

 (with-private-fns [datatype.core [lazy-pattern?]]
     (deftest we-can-detect-lazy-patterns
         (is (lazy-pattern? 'datatype.core-tests/lexpr))
         (is (lazy-pattern? '[datatype.core-tests/lctor x y]))
         (is (lazy-pattern? 'datatype.core-tests/lexpr2))
         (is (lazy-pattern? '[datatype.core-tests/lctor2 x y]))))
 )
