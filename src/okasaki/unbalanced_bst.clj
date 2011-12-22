(ns okasaki.unbalanced-bst
    (:use ml.datatype))

(defdatatype
    ::UnbalancedBST
    Empty         ; empty tree
    (Node a x b)) ; tree with root x, left subtree a and right subtree b
    
(defun insert [x t]
    [x Empty] 
        (Node Empty x Empty)
    [x ([Node a y b] :as s)]
        (cond 
            (< x y) (Node (insert x a) y b)
            (< y x) (Node a y (insert x b))
            :else   s))

(defun member [x t]
    [_ Empty]
        false
    [x [Node a y b]]
        (cond
            (< x y) (recur x a)
            (< y x) (recur x b)
            :else   true))
