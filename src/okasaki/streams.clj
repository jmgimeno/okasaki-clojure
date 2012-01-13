(ns okasaki.streams
    (:use datatype.core))

(defdatatype
    ::StreamCell
    Nil
    (Cons first rest))

;; Streams are ($ StreamCells)

(defun s-first
    ;;[stream]
    [($ [Cons x _])] x)

(defunlazy s-rest
    ;;[stream]
    [($ [Cons _ r])] r)

(defunlazy s-append
    ;;[stream1 stream2]
    [($ Nil)        t] t
    [($ [Cons x s]) t] ($ (->Cons x (s-append s t))))

(defunlazy s-take
    ;;[number stream]
    [0 s             ] ($ Nil)
    [_ ($ Nil)       ] ($ Nil)
    [n ($ [Cons x s])] ($ (->Cons x (s-take (dec n) s))))

(defun s-drop_
    ;;[number stream]
    [0 s] s
    [n ($ Nil)] ($ Nil)
    [n ($ [Cons x s])] (recur (dec n) s))

(defunlazy s-drop
    ;;[number stream]
    [n s] (s-drop_ n s))

(defun s-reverse_
    ;;[stream result]
    [($ Nil)        r] r
    [($ [Cons x s]) r] (recur s ($ (->Cons x r))))

(defunlazy s-reverse
    ;;[stream]
    [s] (s-reverse_ s ($ Nil)))

