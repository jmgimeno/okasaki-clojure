# okasaki-clojure

An implementation of some data structures described in Okasaki's book "Purely 
Functional Data Structures".

I'm trying to follow _almost directly_ the ML implementations using some sugar on David Nolen's [core.match]
(https://github.com/clojure/core.match) library. 

This _sugar_ is defined in namespace datatype.core.

## defdatatype

A datatype contains an object to name it, and a group of constructors
with or without parameters.

For instance, a _datatype_ for unbalanced binary search trees can be defined as:

    (defdatatype
        ::UnbalancedBST
        Empty        
        (Node left root right)) 
        
Two kinds of _constructors_ are possible: 

* _Constants_, which are represented as symbols bound to the
corresponding keyword (e.g. Empty to :user/Empty) 
both in the current namespace.

* _Factories_, which are represented as records with the same name
as given  and corresponding fields. Field names are mangled in order to
not have conficts among different constructors using the same names.
For instance, in the example, fields are named `node-left`,
`node-root` and `node-right`.
  
## defun

We can now define  _functions_ using pairs of patterns and actions:

    (defun insert
        [x Empty] 
            (->Node Empty x Empty)
        [x [Node a y b]]
            (cond 
                (< x y) (->Node (insert x a) y b)
                (< y x) (->Node a y (insert x b))
                :else    t))

    (defun member
        [_ Empty]
            false
        [x [Node a y b]]
            (cond
                (< x y) (recur x a)
                (< y x) (recur x b)
                :else   true))

As _factory constructors_ are defined as records, we can use `->Node`,
`:node-left`, `:node-root`, `:node-right` on the actions.

### as-patterns

A special _as-pattern_ allows capturing the whole value of a parameter
besides its parts. For instance:

    (defun insert
        [x Empty] 
            (->Node Empty x Empty)
        [x ([Node a y b] :as t)]
            (cond 
                (< x y) (->Node (insert x a) y b)
                (< y x) (->Node a y (insert x b))
                :else  t))
                
### or-patterns

We can define _or-patterns_ to group conditions that correspond to the
same action. They only work at the topmost level of a condition. For
instance:

    (defun ^:private balance
        (:or [Black [Node Red [Node Red a x b] y c] z d] 
              [Black [Node Red a x [Node Red b y c]] z d] 
              [Black a x [Node Red [Node Red b y c] z d]] 
              [Black a x [Node Red b y [Node Red c z d]]]) 
                  (->Node Red (->Node Black a x b) y (->Node Black c z d))
        [c a x b]                                         
                  (->Node c a x b))

## caseof

We can also define conditional code using patterns by means of the `caseof`
macro:

    (caseof [t]
        [Empty] true
        [[Node _ _ _]] false)

## $-notation

In chapter 4 of the book $-notation is presented to allow
lazy-evaluation. We have defined symbol `$ with two 
complementary meanings, depending on the side of the rule where it appears:

* When used in the _action_ part of the rule `($ expr)` is completely equivalent to `(delay expr)`
* In a pattern, we have that `($ pattern)` matched expr when pattern matches `(force expr)`

For instance, we can define Streams as delayed StreamCells and define:

    (defun s-drop_
        [0 s] s
        [n ($ Nil)] ($ Nil)
        [n ($ [Cons x s])] (recur (dec n) s))

## defunlazy

In order to simplify the construction of lazy-functions (which return delayed objects) we have defined the 
_equivalent_ macro using the definition given in the book:

    fun lazy f p = e 

is equivalent to

    fun f x = $case x of p => force e

For instance, we can define now:

    (defunlazy s-append
        [($ Nil) t] t
        [($ [Cons x s]) t] ($ (->Cons x (s-append s t))))

and the streams are not evaluated when applied but when accessed.


## Some simplifications

An alternartive to $-notation has been defined to simplify some lazy definitions and datatypes.

### Lazy constructors

Some constructors can be lazy: they are evaluated only if needed. 

To define a constructor as lazy, we associate the :lazy metadata
as true with it.

For instance, we can define the Streams type as

    (defdatatype
        ::Streams
        Nil
        (^:datatype.core/lazy Cons elem stream))

which would define the Cons constructor to be lazy.

Using it we can define a function that returns the infinite stream of naturals

    (defn nats
        ([]  (nats 0))
        ([n] (->Cons n (nats (inc n)))))
          
### deflazy

When we want to define all the constructors of a datatype as lazy, we
can use deflazy as a shortcut.

    (deflazy
        ::Streams
        Nil
        (Cons elem stream))

and now both constructors would be defined as lazy.

_(c) Juan Manuel Gimeno Illa_
