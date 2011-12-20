(ns ml.adt
    (:use [clojure.core.match :only [match]]))

;; Inspired by Konrad Hinsen's clojure.contrib.types

(defn- symbol-to-keyword
    [s]
    (keyword (str *ns*) (str s)))

(defn- constructor-name [constructor]
    (if (symbol? constructor) 
        constructor 
        (first constructor)))

(defn- constructor-value [constructor]
    (if (symbol? constructor)
        (symbol-to-keyword constructor)
        (let [[name & args] constructor]
            `(fn [~@args]
                [~(symbol-to-keyword name) ~@args]))))

(defn- make-constructor
    [type constructor]
    (let [name  (constructor-name constructor)
          value (constructor-value constructor)]
          `(def ~(with-meta name {::adt type}) ~value)))

(defmacro defdata
    [type & constructors]
    `(do
        ~@(map (partial make-constructor type) constructors)))
        
(defmacro defun
    [name args & eqs]
    `(defn ~name ~args
        (match [~(symbol-to-keyword name) ~@args]
            ~@eqs)))