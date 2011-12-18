(ns okasaki.bs-tree
    (:use [clojure.core.match :only [match]])
    (:use okasaki.adt))

(defadt 
    ::BST     ; name
    E         ; empty tree
    (T a x b) ; tree with root x, left subtree a and right subtree b
)
    
(defeqs insert [x t]
    [::insert x ::E] 
        (T E x E)
    [::insert x ([::T a y b] :as s)]
        (cond 
            (< x y) (T (insert x a) y b)
            (< y x) (T a y (insert x b))
            :else   s))

(defeqs member [x t]
    [::member _ ::E]
        false
    [::member x [::T a y b]]
        (cond
            (< x y) (member x a)
            (< y x) (member x b)
            :else   true))
