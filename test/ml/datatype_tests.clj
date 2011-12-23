(ns ml.datatype-tests
    (:use ml.datatype)
    (:use clojure.test))

(defdatatype ::type expr (ctor arg1 arg2))

(deftest empty-constructor-returns-keyword-in-current-namespace
    (is (= ::expr expr)))

(deftest non-empty-constructor-returns-vector
    (is (= [::ctor 'x 'y] (ctor 'x 'y))))

(deftest constructors-have-the-right-metadata
    (is (= ::type (:ml.datatype/datatype (meta #'expr))))
    (is (= ::type (:ml.datatype/datatype (meta #'ctor)))))

(deftest case-of-works-properly
    (let [test #(caseof [%] [expr] 0 [[ctor x y]] (+ x y))]
	(is (= 0 (test expr)))
	(is (= 3 (test (ctor 1 2))))))


