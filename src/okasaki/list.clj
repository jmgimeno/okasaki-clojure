(ns okasaki.list
    (:use datatype.core))

(defdatatype
    ::List
    Nil
    (Cons elem list))

(defun append [ls1 ls2]
    [Nil ls2] ls2
    [[Cons l1 ls1_] ls2] (Cons l1 (append ls1_ ls2)))

(defun rev [ts]
    [Nil] Nil
    [[Cons t ts_]] (append (rev ts_) (Cons t Nil)))