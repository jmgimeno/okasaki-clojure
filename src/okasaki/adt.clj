(ns okasaki.adt)

;; Inspired by Konrad Hinsen's clojure.contrib.types

(defn- make-tag
    [s]
    (keyword (str *ns*) (str s)))

(defn- make-constructor
    [constructor]
    (if (symbol? constructor)
        `(def ~constructor ~(make-tag constructor))
        (let [[name & args] constructor]
            `(defn ~name [~@args]
                [~(make-tag name) ~@args]))))

(defmacro defcons
    [& constructors]
    `(do
        ~@(map make-constructor constructors)))
        
(defmacro defeqs
    [name args & eqs]
    `(defn ~name ~args
        (~'match [~(make-tag name) ~@args]
            ~@eqs)))