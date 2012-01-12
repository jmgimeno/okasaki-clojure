(ns okasaki.unbalanced-bst
    (:use datatype.core))

(defdatatype
    ::UnbalancedBST
    Empty         
    (Node left root right))
    
(defun insert
    [elem tree]
    [x Empty] 
        (->Node Empty x Empty)
    [x ([Node a y b] :as t)]
        (cond 
            (< x y) (->Node (insert x a) y b)
            (< y x) (->Node a y (insert x b))
            :else   t))

(defun member
    [elem tree]
    [_ Empty]
        false
    [x [Node a y b]]
        (cond
            (< x y) (recur x a)
            (< y x) (recur x b)
            :else   true))
