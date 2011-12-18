(ns ml.adt
    (:use [clojure.core.match :only [match]]))

;; Inspired by Konrad Hinsen's clojure.contrib.types

(defn symbol-to-keyword
    [s]
    (keyword (str *ns*) (str s)))

(defn make-constructor
    [type constructor]
    (if (symbol? constructor)
        `(defconstructor
            ~type
            ~constructor 
            ~(symbol-to-keyword constructor))
        (let [[name & args] constructor]
            `(defconstructor
                ~type
                ~name 
                [~@args] 
                [~(symbol-to-keyword name) ~@args]))))

(defmacro defconstructor
    ([type name expr]
        `(def ~(with-meta name (assoc (meta name) ::adt type)) ~expr))
    ([type name args body]
        `(defn ~(with-meta name (assoc (meta name) ::adt type)) ~args ~body)))

(defmacro defadt
    [type & constructors]
    `(do
        ~@(map (partial make-constructor type) constructors)))
        
(defmacro defequations
    [name args & eqs]
    `(defn ~name ~args
        (clojure.core.match/match [~(symbol-to-keyword name) ~@args]
            ~@eqs)))