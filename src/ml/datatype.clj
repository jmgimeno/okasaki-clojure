
(ns ml.datatype
    (:use [clojure.core.match :only [match]]
          [clojure.walk :only [postwalk]]))

;; Inspired by Konrad Hinsen's clojure.contrib.types

(defn- symbol-to-keyword
    [s]
    (keyword (str *ns*) (str s)))

(defn- make-constructor-eager
    [type constructor]
    (if (symbol? constructor)
        `(def ~(with-meta constructor {::datatype type})
             ~(symbol-to-keyword constructor))
        (let [[name & args] constructor]
            `(defn ~(with-meta name {::datatype type}) [~@args]
                 [~(symbol-to-keyword name) ~@args]))))

(defmacro defdatatype
    [type & constructors]
    `(do
        ~@(map (partial make-constructor-eager type) constructors)))

(defn- make-constructor-lazy
    [type constructor]
    (if (symbol? constructor)
        `(def ~(with-meta constructor {::datatype type ::lazy true})
             (delay ~(symbol-to-keyword constructor)))
        (let [[name & args] constructor]
            `(defmacro ~(with-meta name {::datatype type ::lazy true}) [~@args]
                 (list 'delay [~(symbol-to-keyword name) ~@args])))))

(defn- lazy?
    [pattern]
    (cond (symbol? pattern) (contains? (meta (resolve pattern)) ::lazy)
          (vector? pattern) (lazy? (first pattern))))

(defmacro deflazy
    [type & constructors]
    `(do
         ~@(map (partial make-constructor-lazy type) constructors)))

(defn- constructor?
    [s]
    (and (symbol? s) (contains? (meta (resolve s)) ::datatype)))
    
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
          rows              (map first  row-action-pairs)
          transposed-rows   (apply map vector rows)
          need-force        (map #(some lazy? %) transposed-rows)
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

