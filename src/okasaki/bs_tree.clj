(ns okasaki.bs-tree
    (:use [clojure.core.match :only [match]]))

(def E ::E)

(defn T [a x b]
    [::T a x b])

(defn insert [x t]
    (match [::insert x t]
        [::insert x ::E] 
            (T E x E)
        [::insert x ([::T a y b] :as s)]
            (cond 
                (< x y) (T (insert x a) y b)
                (< y x) (T a y (insert x b))
                :else   s)))
    
(defn member [x t]
    (match [::member x t]
        [::member _ ::E]
            false
        [::member x [::T a y b]]
            (cond
                (< x y) (member x a)
                (< y x) (member x b)
                :else   true)))
