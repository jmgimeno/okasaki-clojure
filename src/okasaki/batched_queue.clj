(ns okasaki.batched-queue
    (:refer-clojure :exclude [empty? split-at])
    (:use datatype.core
          okasaki.list))

(defdatatype
    ::BatchedQueue
    (Pair front rear))

(def Empty (->Pair Nil Nil))

(defun empty?
    ;;[queue]
    [[Pair Nil _]] true
    :else          false)

(defun checkf
    ;;[queue]
    [[Pair Nil r]] (->Pair (rev r) Nil)
    [q]            q)

(defun snoc
    ;;[queue elem]
    [[Pair f r] x] (checkf (->Pair f (->Cons x r))))

(defun head
    ;;[queue]
    [[Pair Nil _]]        (throw (IllegalStateException. "Attempting head of an empty queue"))
    [[Pair [Cons x _] _]] x)

(defun tail
    ;;[queue]
    [[Pair Nil _]]        (throw (IllegalStateException. "Attempting head of an empty queue"))
    [[Pair [Cons _ f] r]] (checkf (->Pair f r)))


