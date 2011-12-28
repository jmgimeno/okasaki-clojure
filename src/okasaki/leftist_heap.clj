(ns okasaki.leftist-heap
    (:refer-clojure :exclude [merge])
    (:use datatype.core))
    
(defdatatype
    ::LeftistHeap
    Empty
    (Node r x a b))

(declare merge rank makeT insert findMin deleteMin)

(defun merge [h1 h2]
    [h Empty] 
        h
    [Empty h]
        h
    [([Node _ x a1 b1] :as h1) ([Node _ y a2 b2] :as h2)]
        (if (< x y)
            (makeT x a1 (merge b1 h2))
            (makeT y a2 (merge h1 b2))))
            
(defun rank [h]
    [Empty]           
        0
    [[Node r _ _ _]] 
        r)
    
(defn makeT [x a b]
    (if (>= (rank a) (rank b))
        (Node (inc (rank b)) x a b)
        (Node (inc (rank a)) x b a)))
        
(defn insert [x h]
    (merge (Node 1 x Empty Empty) h))
    
(defun findMin [h]
    [[Node _ x _ _]] 
        x)
    
(defun deleteMin [h]
    [[Node _ _ a b]] 
        (merge a b))
