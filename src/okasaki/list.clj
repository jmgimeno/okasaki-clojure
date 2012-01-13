(ns okasaki.list
    (:refer-clojure :exclude [split-at])
    (:use datatype.core))

(defdatatype
    ::List
    Nil
    (Cons elem list))

(defun append
    ;;[list1 list2]
    [Nil ls2] ls2
    [[Cons l1 ls1_] ls2] (->Cons l1 (append ls1_ ls2)))

(defun rev
    ;;[list]
    [Nil] Nil
    [[Cons t ts_]] (append (rev ts_) (->Cons t Nil)))

(defun split-at
    ;;[num list]
    [0 l] [Nil l]
    [_ Nil] [Nil Nil]
    [n [Cons h t]] (let [[f r] (split-at (dec n) t)]
                       [(->Cons h f) r]))