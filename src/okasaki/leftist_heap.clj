(ns okasaki.leftist-heap
    (:refer-clojure :exclude [merge])
    (:use ml.adt))
    
(defdata
    ::LeftistHeap
    E
    (T r x a b))

(declare merge rank makeT insert findMin deleteMin)

(defun merge [h1 h2]
    [h ::E] 
        h
    [::E h]
        h
    [([::T _ x a1 b1] :as h1) ([::T _ y a2 b2] :as h2)]
        (if (< x y)
            (makeT x a1 (merge b1 h2))
            (makeT y a2 (merge h1 b2))))
            
(defun rank [h]
    [::E]           
        0
    [[::T r _ _ _]] 
        r)
    
(defn makeT [x a b]
    (if (>= (rank a) (rank b))
        (T (inc (rank b)) x a b)
        (T (inc (rank a)) x b a)))
        
(defn insert [x h]
    (merge (T 1 x E E) h))
    
(defun findMin [h]
    [[::T _ x _ _]] 
        x)
    
(defun deleteMin [h]
    [[::T _ _ a b]] 
        (merge a b))
