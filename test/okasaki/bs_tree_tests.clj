(ns okasaki.bs-tree-tests
    (:use okasaki.bs-tree)
    (:use midje.sweet))
    
(fact "empty tree"
    (member 1 E) => falsey)

(facts "one node tree"
    (member 1 (insert 2 E)) => falsey
    (member 2 (insert 1 E)) => falsey
    (member 1 (insert 1 E)) => truthy)
    
(facts "two-nodes tree"
    (member 1 (insert 1 (insert 2 E))) => truthy
    (member 1 (insert 2 (insert 1 E))) => truthy
    (member 3 (insert 1 (insert 2 E))) => falsey
    (member 0 (insert 1 (insert 2 E))) => falsey)
