(ns okasaki.batched-deque
    (:refer-clojure :exclude [cons empty? last])
    (:use datatype.core
          okasaki.list))

;; Solution to exercice 5.1

(defdatatype
    ::Deque
    (Quad cfront front crear rear))

(def Empty (->Quad 0 Nil 0 Nil))

(defun empty?
    [queue]
    [[Quad 0 Nil 0 Nil]] true
    :else                false)

(defun equalize
    [queue]
    [[Quad c1 [Cons h1 t1] c2 [Cons h2 t2]]] (let [diff (- c1 c2)]
                                                 (cond (< diff -1) (recur (->Quad (inc c1) (->Cons h2 (->Cons h1 t1)) (dec c2) t2))
                                                       (> diff +1) (recur (->Quad (dec c1) t1 (inc c2) (->Cons h1 (->Cons h2 t2))))
                                                       :else       queue)))

(defun checkf
    [queue]
    [[Quad 0 Nil 1 _]] queue
    [[Quad 1 _ 0 Nil]] queue
    [[Quad 0 Nil c [Cons h t]]] (caseof
                                 [(equalize (->Quad 1 (->Cons h Nil) (dec c) t))]
                                 [[Quad cf f cr r]] (->Quad cr (rev r) cf (rev f)))
    [[Quad c [Cons h t] 0 Nil]] (caseof
                                 [(equalize (->Quad (dec c) t 1 (->Cons h Nil)))]
                                 [[Quad cf f cr r]] (->Quad cr (rev r) cf (rev f)))
    :else                       queue)

(defun cons
    [elem deque]
    [x [Quad cf f cr r]] (checkf (->Quad (inc cf) (->Cons x f) cr r)))

(defun head
    [queue]
    [[Quad 0 Nil 0 Nil]]          (throw (IllegalStateException. "Attempting head of an empty queue"))
    [[Quad _ [Cons h _] _ _]]     h
    [[Quad 0 Nil 1 [Cons h Nil]]] h)

(defun tail
    [queue]
    [[Quad 0 Nil 0 Nil]]        (throw (IllegalStateException. "Attempting tail of an empty queue"))
    [[Quad c [Cons h t] cr r]]  (checkf (->Quad (dec c) t cr r))
    [[Quad 0 Nil c [Cons h t]]] (checkf (->Quad 0 Nil (dec c) t)))

(defun snoc
    [elem queue]
    [x [Quad cf f cr r]] (checkf (->Quad cf f (inc cr) (->Cons x r))))

(defun last
    [queue]
    [[Quad 0 Nil 0 Nil]]        (throw (IllegalStateException. "Attempting last of an empty queue"))
    [[Quad _ _ _ [Cons h t]]]   h
    [[Quad 1 [Cons h Nil] 0 Nil]] h)

(defun init
    [queue]
    [[Quad 0 Nil 0 Nil]]        (throw (IllegalStateException. "Attempting init of an empty queue"))
    [[Quad cf f cr [Cons h t]]] (checkf (->Quad cf f (dec cr) t))
    [[Quad c [Cons h t] 0 Nil]] (checkf (->Quad (dec c) t 0 Nil)))
