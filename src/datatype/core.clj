(ns datatype.core
    (:use [clojure.core.match :only [match]]
          [clojure.walk :only [postwalk]]))

(defn- symbol-to-keyword
    [s]
    (keyword (str *ns*) (str s)))

(defn- make-constructor-eager
    [type constructor]
    (if (symbol? constructor)
        `(def ~(vary-meta  constructor assoc :datatype type)
             ~(symbol-to-keyword constructor))
        (let [[name & args] constructor]
            `(defn ~(vary-meta name assoc :datatype type) [~@args]
                 [~(symbol-to-keyword name) ~@args]))))

(defn- make-constructor-lazy
    [type constructor]
    (if (symbol? constructor)
        `(def ~(vary-meta  constructor assoc :datatype type :lazy true)
             (delay ~(symbol-to-keyword constructor)))
        (let [[name & args] constructor]
            `(defmacro ~(vary-meta name assoc :datatype type :lazy true) [~@args]
                 (list 'delay [~(symbol-to-keyword name) ~@args])))))

(defn- lazy-constructor?
    [constructor]
    (cond (symbol? constructor) (:lazy (meta constructor))
          (list? constructor) (lazy-constructor? (first constructor))))
    
(defn- make-constructor
    [type lazy constructor]
    (if (or lazy (lazy-constructor? constructor))
        (make-constructor-lazy type constructor)
        (make-constructor-eager type constructor)))

(defn- lazy-pattern?
    [pattern]
    (cond (symbol? pattern) (:lazy (meta (resolve pattern)))
          (vector? pattern) (lazy-pattern? (first pattern))))

(defmacro defdatatype
    [type & constructors]
    `(do
         ~@(map (partial make-constructor type false) constructors)))

(defmacro deflazy
    [type & constructors]
    `(do
         ~@(map (partial make-constructor type true) constructors)))

(defn- constructor?
    [s]
    (and (symbol? s) (contains? (meta (resolve s)) :datatype)))
    
(defn- transform-row
    [pattern]
    (postwalk #(if (constructor? %) (symbol-to-keyword %) %) pattern))

(defn- transform-arg
    [arg needs-force]
    (if needs-force
        `(force ~arg)
        arg))

(defmacro caseof
    [args & rules]
    (let [row-action-pairs  (partition 2 rules)
          rows              (map first row-action-pairs)
          transposed-rows   (apply map vector rows)
          need-force        (map #(some lazy-pattern? %) transposed-rows)
          actions           (map second row-action-pairs)
          transformed-args  (map transform-arg args need-force)
          transformed-rows  (map transform-row rows)
          transformed-rules (interleave transformed-rows actions)]
        `(match ~(vec transformed-args)
                ~@transformed-rules)))

(defmacro defun
    [name args & rules]
    `(defn ~name ~args
         (caseof ~args
                 ~@rules)))
