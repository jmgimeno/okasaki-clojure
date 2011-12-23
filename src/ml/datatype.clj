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

(defn- constructor?
    [s]
    (and (symbol? s) (contains? (meta (resolve s)) ::datatype)))
    
(defn- transform-row
    [pattern]
    (postwalk #(if (constructor? %) (symbol-to-keyword %) %) pattern))

(defmacro caseof
    [occurrences & rules]
    (let [row-action-pairs  (partition 2 rules)
          rows              (map first  row-action-pairs)
          actions           (map second row-action-pairs)
          transformed-rows  (map transform-row rows)
          transformed-rules (interleave transformed-rows actions)]
        `(match ~occurrences
                ~@transformed-rules)))

(defmacro defun
    [name args & rules]
    `(defn ~name ~args
         (caseof ~args
                 ~@rules)))

