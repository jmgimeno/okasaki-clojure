(ns okasaki.binomial-heap
    (:refer-clojure :exclude [merge split-at])
    (:use datatype.core
          okasaki.list))

(defdatatype
    ::Tree
    (Node rank root treelist))

(defun link
    [tree1 tree2]
    [([Node r x1 c1] :as t1) ([Node _ x2 c2] :as t2)] (if (< x1 x2)
                                                          (->Node (inc r) x1 (->Cons t2 c1))
                                                          (->Node (inc r) x2 (->Cons t1 c2))))

(defun insTree
    [tree heap]
    [t Nil]                    (->Cons t Nil)
    [t ([Cons t_ ts_] :as ts)] (if (< (:node-rank t) (:node-rank t_))
                                   (->Cons t ts)
                                   (recur (link t t_) ts_)))

(defn insert
    [elem heap]
    (insTree (->Node 0 elem Nil) heap))

(defun merge
    [heap1 heap2]
    [ts1 Nil] ts1
    [Nil ts2] ts2
    [([Cons t1 ts1_] :as ts1) ([Cons t2 ts2_] :as ts2)] (cond (< (:node-rank t1) (:node-rank t2)) (->Cons t1 (merge ts1_ ts2))
                                                              (> (:node-rank t1) (:node-rank t2)) (->Cons t2 (merge ts1 ts2_))
                                                              :else (insTree (link t1 t2) (merge ts1_ ts2_))))

(defun removeMinTree
    [heap]
    [[Cons t Nil]] [t Nil]
    [[Cons t ts]] (let [[t_ ts_] (removeMinTree ts)]
                      (if (< (:node-root t) (:node-root t_))
                          [t ts]
                          [t_ (->Cons t ts_)])))

(defn findMin
    [heap]
    (let [[t _] (removeMinTree heap)]
        (:node-root t)))

(defn deleteMin
    [heap]
    (let [[t ts] (removeMinTree heap)]
        (caseof [t]
                [[Node _ x ts_]] (merge (rev ts_) ts))))

