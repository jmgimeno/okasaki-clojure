(ns ml.adt-tests
    (:use ml.adt)
    (:use midje.sweet))
    
(fact "we can create keywords for symbols"
    (symbol-to-keyword 'lala) 
        => :ml.adt-tests/lala)
 
(fact "we can create the correct constructor for symbols"
    (make-constructor ::type 'ctor)
        => '(ml.adt/defconstructor ::type ctor ::ctor))
    
(fact "we can create the correct constructor for expressions"
    (make-constructor ::type '(ctor arg1 arg2 arg3)) 
        => '(ml.adt/defconstructor ::type ctor [arg1 arg2 arg3] [::ctor arg1 arg2 arg3]))
        
(fact "we expand an adt with a single symbol"
    (macroexpand-1 '(defadt ::type e)) 
        => '(do (ml.adt/defconstructor ::type e ::e)))
    
(fact "we expand an adt with a single expression"
    (macroexpand-1 '(defadt ::type (ctor arg1))) 
        => '(do (ml.adt/defconstructor ::type ctor [arg1] [::ctor arg1])))
        
(fact "we expand an adt with symbols and expressions"
    (macroexpand-1 '(defadt ::type e (ctor arg1)))
        => '(do 
                (ml.adt/defconstructor ::type e ::e)
                (ml.adt/defconstructor ::type ctor [arg1] [::ctor arg1])))
                
(defadt ::type expr (ctor arg1 arg2))

(fact "expr returns kayword in current namespace"
    expr => ::expr)
    
(fact "ctor is a function whit return a vector expression"
    (ctor ...arg1... ...arg2...) => [::ctor ...arg1... ...arg2...])
    
(facts "constructors have metadata set"
    (meta #'expr) => (contains {:ml.adt/adt ::type})
    (meta #'ctor) => (contains {:ml.adt/adt ::type}))
