(ns okasaki.adt)

;; Inspired by Konrad Hinsen's clojure.contrib.types

(defn symbol-to-keyword
    [s]
    (keyword (str *ns*) (str s)))

(defn make-constructor
    [type constructor]
    (if (symbol? constructor)
        `(def 
            ~constructor 
            ~(symbol-to-keyword constructor))
        (let [[name & args] constructor]
            `(defn
                ~name 
                [~@args] 
                [~(symbol-to-keyword name) ~@args]))))

(defmacro defadt
    [type & constructors]
    `(do
        ~@(map (partial make-constructor type) constructors)))
        
(defmacro defeqs
    [name args & eqs]
    `(defn ~name ~args
        (~'match [~(symbol-to-keyword name) ~@args]
            ~@eqs)))