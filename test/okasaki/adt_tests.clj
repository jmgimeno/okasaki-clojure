(ns okasaki.adt-tests
    (:use okasaki.adt)
    (:use midje.sweet))
    
(fact "we can create keywords for symbols"
    (symbol-to-keyword 'lala) 
        => :okasaki.adt-tests/lala)
 
(fact "we can create the correct constructor for symbols"
    (make-constructor ::type 'ctor)
        => '(def ctor ::ctor))
    
(fact "we can create the correct constructor for expressions"
    (make-constructor ::type '(ctor arg1 arg2 arg3)) 
        => '(clojure.core/defn ctor [arg1 arg2 arg3] [::ctor arg1 arg2 arg3]))
        
(fact "we expand an adt with a single symbol"
    (macroexpand-1 '(defadt ::type e)) 
        => '(do (def e ::e)))
    
(fact "we expand an adt with a single expression"
    (macroexpand-1 '(defadt ::type (ctor arg1))) 
        => '(do (clojure.core/defn ctor [arg1] [::ctor arg1])))
        
(fact "we expand an adt with symbols and expressions"
    (macroexpand-1 '(defadt ::type e (ctor arg1)))
        => '(do 
                (def e ::e)
                (clojure.core/defn ctor [arg1] [::ctor arg1])))
                
(defadt ::type expr (ctor arg1 arg2))

(fact "expr returns kayword in current namespace"
    expr => :okasaki.adt-tests/expr)
    
(fact "ctor is a function whit return a vector expression"
    (ctor ...arg1... ...arg2...) => [:okasaki.adt-tests/ctor ...arg1... ...arg2...])
