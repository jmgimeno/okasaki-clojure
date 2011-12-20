(ns ml.adt-tests
    (:use ml.adt)
    (:use midje.sweet))

(defdata ::type expr (ctor arg1 arg2))

(fact "expr returns kayword in current namespace"
    expr => ::expr)
    
(fact "ctor is a function whit return a vector expression"
    (ctor ...arg1... ...arg2...) => [::ctor ...arg1... ...arg2...])
    
(facts "constructors have metadata set"
    (meta #'expr) => (contains {:ml.adt/adt ::type})
    (meta #'ctor) => (contains {:ml.adt/adt ::type}))
