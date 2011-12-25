(ns okasaki.streams
    (use ml.datatype))

(deflazy
    ::Streams
    Nil
    (Cons elem stream))

(defun s-first [stream]
    [[Cons x _]] x)

(defun s-rest [stream]
    [[Cons _ r]] r)

(defun s-take [number stream]
    [0 s]          s
    [_ Nil]        Nil
    [n [Cons x s]] (Cons x (take (dec n) s)))

(defun s-drop [number stream]
    [0 s]          s
    [_ Nil]        Nil
    [n [Cons _ s]] (recur (dec n) s))
