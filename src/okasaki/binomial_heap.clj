(ns okasaki.binomial-heap
    (:refer-clojure :exclude [merge])
    (:use datatype.core))

(defdatatype
    ::TreeList
    Nil
    (Cons tree treelist))

(defun append [ls1 ls2]
    [Nil ls2] ls2
    [[Cons l1 ls1_] ls2] (Cons l1 (append ls1_ ls2)))

(defun rev [ts]
    [Nil] Nil
    [[Cons t ts_]] (append (rev ts_) (Cons t Nil)))

(defdatatype
    ::Tree
    (Node rank elem treelist))

(defun rank [t]
    [[Node r _ _]] r)

(defun root [t]
    [[Node _ x _]] x)

(defun link [t1, t2]
    [[Node r x1 c1] [Node _ x2 c2]] (if (< x1 x2)
                                        (Node (inc r) x1 (Cons t2 c1))
                                        (Node (inc r) x2 (Cons t1 c2))))

(defun insTree [t ts]
    [t Nil] (Cons t Nil)
    [t [Cons t_ ts_]] (if (< (rank t) (rank t_))
                          (Cons t ts)
                          (recur (link t t_) ts_)))

(defn insert [x h]
    (insTree (Node 0 x Nil) h))

(defun merge [ts1 ts2]
    [ts1 Nil] ts1
    [Nil ts2] ts2
    [[Cons t1 ts1_] [Cons t2 ts2_]] (cond (< (rank t1) (rank t2)) (Cons t1 (merge ts1_ ts2))
                                          (> (rank t1) (rank t2)) (Cons t2 (merge ts1 ts2_))
                                          :else (insTree (link t1 t2) (merge ts1_ ts2_))))

(defun removeMinTree [s]
    [[Cons t Nil]] [t Nil]
    [[Cons t ts]] (let [[t_ ts_] (removeMinTree ts)]
                      (if (< (root t) (root t_))
                          [t ts]
                          [t_ (Cons t ts_)])))

(defn findMin [h]
    (let [[t _] (removeMinTree h)]
        (root t)))

(defn deleteMin [h]
    (let [[t ts] (removeMinTree h)]
        (caseof [t]
                [[Node _ x ts_]] (merge (rev ts_) ts))))

