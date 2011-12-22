(ns okasaki.unbalanced-bst-tests
    (:use okasaki.unbalanced-bst)
    (:use clojure.test))

(deftest tree-without-nodes
    (is (not (member 1 Empty))))

(deftest one-node-tree
    (is (member 1 (insert 1 Empty)))
    (is (not (member 1 (insert 2 Empty))))
    (is (not (member 2 (insert 1 Empty)))))

(deftest two-nodes-tree
    (is (member 1 (insert 1 (insert 2 Empty))))
    (is (member 1 (insert 2 (insert 1 Empty))))
    (is (not (member 3 (insert 1 (insert 2 Empty)))))
    (is (not (member 0 (insert 1 (insert 2 Empty))))))    

