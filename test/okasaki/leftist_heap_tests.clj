(ns okasaki.leftist-heap-tests
    (:use [okasaki.leftist-heap :exclude [merge]])
    (:use midje.sweet))
    
(fact "test the minimum for one element heap"
    (findMin (insert 1 E)) => 1)
    
(facts "test the minimum for two elements heap"
    (findMin (insert 1 (insert 2 E))) => 1
    (findMin (insert 2 (insert 1 E))) => 1)
    
(facts "test the second minimum after removing the minimum"
    (findMin (deleteMin (insert 1 (insert 2 E)))) => 2
    (findMin (deleteMin (insert 2 (insert 1 E)))) => 2)