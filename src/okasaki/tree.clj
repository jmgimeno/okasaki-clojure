(ns okasaki.tree
    (:use [clojure.core.match :only [match]]))

(defn tree 
    ([]      [::empty])
    ([l v r] [::node l v r]))
    
(defn is-empty? [t]
    (match t
        [::empty] true
        :else     false))


