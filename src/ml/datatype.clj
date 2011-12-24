(ns ml.datatype
    (:use [clojure.core.match :only [match]]
          [clojure.walk :only [postwalk]]))

;; Inspired by Konrad Hinsen's clojure.contrib.types

(defn- symbol-to-keyword
    [s]
    (keyword (str *ns*) (str s)))

(defn- constructor-name 
    [constructor]
    (if (symbol? constructor) 
        constructor 
        (first constructor)))

(defn- constructor-value
    [constructor]
    (if (symbol? constructor)
        (symbol-to-keyword constructor)
        (let [[name & args] constructor]
            `(fn [~@args]
                [~(symbol-to-keyword name) ~@args]))))

(defn- make-constructor
    [type constructor]
    (let [name  (constructor-name constructor)
          value (constructor-value constructor)]
          `(def ~(with-meta name {::datatype type}) ~value)))

(defmacro defdatatype
    [type & constructors]
    `(do
        ~@(map (partial make-constructor type) constructors)))

(defn- constructor-value-lazy
    [constructor]
    (if (symbol? constructor)
        `(delay ~(symbol-to-keyword constructor))
        (let [[name & args] constructor]
            `(fn [~@args]
                (delay [~(symbol-to-keyword name) ~@args])))))

(defn- make-constructor-lazy
    [type constructor]
    (let [name  (constructor-name constructor)
          value (constructor-value-lazy constructor)]
          `(def ~(with-meta name {::datatype type ::lazy true}) ~value)))

(defn lazy?
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

(defn- transform-occurrence
    [occurrence needs-force]
    (if needs-force
        `(force ~occurrence)
        occurrence))

(defmacro caseof
    [occurrences & rules]
    (let [row-action-pairs  (partition 2 rules)
          rows              (map first  row-action-pairs)
          transposed-rows   (apply map vector rows)
          need-force        (map #(some lazy? %) transposed-rows)
          actions           (map second row-action-pairs)
          transformed-occur (map transform-occurrence occurrences need-force)
          transformed-rows  (map transform-row rows)
          transformed-rules (interleave transformed-rows actions)]
        `(match [~@transformed-occur]
                ~@transformed-rules)))

(defmacro defun
    [name args & rules]
    `(defn ~name ~args
         (caseof ~args
                 ~@rules)))

