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
        (is (= 'x (:ctor-arg1 elem)))
        (is (= 'y (:ctor-arg2 elem)))))

(deftest constructors-have-the-right-metadata
    (is (= ::type (:datatype.core/datatype (meta #'expr))))
    (is (= ::type (:datatype.core/datatype (meta #'->ctor)))))

(deftest caseof-works-properly
    (let [test #(caseof [%] [expr] 0 [[ctor x y]] (+ x y))]
	(is (= 0 (test expr)))
	(is (= 3 (test (->ctor 1 2))))))

(deftest we-can-define-else-patterns
    (let [test #(caseof [%] [expr] 0 :else 1)]
        (is (= 0 (test expr)))
        (is (= 1 (test (->ctor 1 2))))))

 (deflazy ::lazy lexpr (lctor arg1 arg2))

 (defdatatype ::lazy2 ^:datatype.core/lazy lexpr2 (^:datatype.core/lazy lctor2 arg1 arg2))

 (deftest empty-constructor-returns-delayed-keyword-in-current-namespace
     (is (delay? lexpr))
     (is (= ::lexpr (force lexpr)))
     (is (delay? lexpr2))
     (is (= ::lexpr2 (force lexpr2))))

 (deftest non-empty-constructor-is-delayed
     (is (delay? (->lctor 'x 'y)))
     (is (delay? (->lctor2 'x 'y))))

 (deftest non-empty-constructor-returns-delayed-record
     (let [record1 (force (->lctor 'x 'y))
           record2 (force (->lctor2 'x 'y))]
         (is (= 'x (:lctor-arg1 record1)))
         (is (= 'y (:lctor-arg2 record1)))
         (is (= 'x (:lctor2-arg1 record2)))
         (is (= 'y (:lctor2-arg2 record2)))))

(deftest caseof-works-properly-with-lazy
    (let [test #(caseof [%] [lexpr] 0 [[lctor x y]] (+ x y))]
        (is (= 0 (test lexpr)))
        (is (= 3 (test (->lctor 1 2))))))

(with-private-fns [datatype.core [lazy-condition?]]
    (deftest we-can-detect-lazy-patterns
        (is (not (lazy-condition? 'datatype.core-tests/expr)))
        (is (not (lazy-condition? '[datatype.core-tests/ctor x y])))
        (is (lazy-condition? 'datatype.core-tests/lexpr))
        (is (lazy-condition? '[datatype.core-tests.lctor x y]))
        (is (lazy-condition? 'datatype.core-tests/lexpr2))
        (is (lazy-condition? '[datatype.core-tests.lctor2 x y]))
        (is (lazy-condition? '($ datatype.core-tests/expr)))
        (is (lazy-condition? '($ [datatype.core-tests/ctor x y])))))

(deftest caseof-works-properly-with-dollar
    (let [test #(caseof [%] [($ expr)] 0 [($ [ctor x y])] (+ x y))]
        (is (= 0 (test ($ expr))))
        (is (= 3 (test ($ (->ctor 1 2)))))))

(defn- counted [counter number]
    (swap! counter inc)
    number)

(defun plus1
    ;;[x y]
    [($ m) ($ n)] ($ (+ m n)))

(deftest plus1-evals-when-applied
    (let [num-evals (atom 0)
          arg1 ($ (counted num-evals 1))
          arg2 ($ (counted num-evals 2))
          sum (plus1 arg1 arg2)]
        (is (= 2 @num-evals))
        (is (= 3 (force sum)))
        (is (= 2 @num-evals))))

(defunlazy plus2
    ;;[x y]
    [($ m) ($ n)] ($ (+ m n)))

(deftest plus1-evals-when-evaluated
    (let [num-evals (atom 0)
          arg1 ($ (counted num-evals 1))
          arg2 ($ (counted num-evals 2))
          sum (plus2 arg1 arg2)]
        (is (= 0 @num-evals))
        (is (= 3 (force sum)))
        (is (= 2 @num-evals))))

(deftest as-patterns
    (let [test #(caseof [%] [([ctor x y] :as b)] [b (+ x y)] [a] a )
          expr1 expr
          expr2 (->ctor 1 2)]
        (is (= expr1 (test expr1)))
        (is (= [expr2 3] (test expr2)))))

(deftest as-patterns-with-lazy-datatypes
    (let [test #(caseof [%] [([lctor x y] :as b)] [b (+ x y)] [a] a )
          lexpr1 lexpr
          lexpr2 (->lctor 1 2)]
        (is (= (force lexpr1) (test lexpr1)))
        (is (= [(force lexpr2) 3] (test lexpr2)))))

(deftest as-patterns-with-dollar-expressions
    (let [test #(caseof [%] [($ expr)] 0 [(($ [ctor x y]) :as all)] [all (+ x y)])
          expr1 ($ expr)
          expr2 ($ (->ctor 1 2))]
        (is (= 0 (test expr1)))
        (is (= [(force expr2) 3] (test expr2)))))
