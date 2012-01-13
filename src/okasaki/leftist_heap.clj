(ns okasaki.leftist-heap
    (:refer-clojure :exclude [merge])
    (:use datatype.core))
    
(defdatatype
    ::LeftistHeap
    Empty
    (Node rank min left right))

(declare merge rank makeT insert findMin deleteMin)

(defun ^:private merge
    ;;[heap1 heap2]
    [h Empty] h
    [Empty h] h
    [([Node _ x a1 b1] :as h1) ([Node _ y a2 b2] :as h2)] (if (< x y)
                                                              (makeT x a1 (merge b1 h2))
                                                              (makeT y a2 (merge h1 b2))))
            
(defun ^:private rank
    ;;[heap]
    [Empty]          0
    [[Node r _ _ _]] r)
    
(defn- makeT
    [x a b]
    (if (>= (rank a) (rank b))
        (->Node (inc (rank b)) x a b)
        (->Node (inc (rank a)) x b a)))
        
(defn insert
    [x h]
    (merge (->Node 1 x Empty Empty) h))
    
(defun findMin
    ;;[heap]
    [[Node _ x _ _]] x)
    
(defun deleteMin
    ;;[heap]
    [[Node _ _ a b]] 
        (merge a b))
