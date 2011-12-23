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

(defn constructor? [s]
    (and (symbol? s) (contains? (meta (resolve s)) ::datatype)))
    
(defn- transform-pattern [pattern]
    (postwalk #(if (constructor? %) (symbol-to-keyword %) %) pattern))

(defn- transform-rules [defs]
    (mapcat (fn [[pattern expr]] [(transform-pattern pattern) expr]) (partition 2 defs)))


(defmacro caseof
    [args & rules]
    `(match ~args
	    ~@(transform-rules rules)))

(defmacro defun
    [name args & rules]
    `(defn ~name ~args
        (caseof ~args
		~@rules)))
