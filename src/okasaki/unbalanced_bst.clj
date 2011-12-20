(ns okasaki.unbalanced-bst
    (:use ml.adt))

(defdata 
    ::UnbalancedBST
    E         ; empty tree
    (T a x b) ; tree with root x, left subtree a and right subtree b
)
    
(defun insert [x t]
    [x ::E] 
        (T E x E)
    [x ([::T a y b] :as s)]
        (cond 
            (< x y) (T (insert x a) y b)
            (< y x) (T a y (insert x b))
            :else   s))

(defun member [x t]
    [_ ::E]
        false
    [x [::T a y b]]
        (cond
            (< x y) (recur x a)
            (< y x) (recur x b)
            :else   true))
