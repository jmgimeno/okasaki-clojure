(ns okasaki.tree-tests
    (:use okasaki.tree)
    (:use midje.sweet))
    
(fact "mk-tree w/o args return an empty tree"
    (tree) => is-empty?
)

(fact "mk-tree with args returns non-empty trees"
    (tree
        (tree)
        1
        (tree)
    ) =not=> is-empty?
)

