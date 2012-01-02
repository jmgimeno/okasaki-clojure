(ns datatype.core
    (:refer-clojure :exclude [replace])
    (:use [clojure.core.match :only [match]]
          [clojure.string :only [join replace split]]))

(defn- symbol-to-keyword
    [s]
    (keyword (str *ns*) (str s)))

(defn- constructor-to-keyword
    [s]
    (keyword (subs (str (resolve s)) 2)))

(defn- constructor-to-factory
    [s]
    (let [parts (split (str s) #"[.]")
          ns (join "." (butlast parts))
          factory (str "->" (last parts))]
        (symbol (if (empty? ns)
                    factory
                    (str ns "/" factory)))))

(defn- qualified-constructor-to-factory
    [s]
    (symbol (replace (str s) #"([^.]+)$" "/->$1")))

(defn- make-constructor-eager
    [type constructor]
    (if (symbol? constructor)
        `(def ~(vary-meta  constructor assoc ::datatype type)
             ~(symbol-to-keyword constructor))
        (let [[name & args] constructor]
            `(do
                 (defrecord ~name [~@args])
                 (alter-meta! (var ~(constructor-to-factory name)) assoc ::datatype ~type)))))

(defn- make-constructor-lazy
    [type constructor]
    (if (symbol? constructor)
        `(def ~(vary-meta  constructor assoc ::datatype type ::lazy true)
             (delay ~(symbol-to-keyword constructor)))
        (let [[name & args] constructor]
            `(do
                 (defrecord ~name [~@args])
                 (defmacro ~(vary-meta (constructor-to-factory name) assoc ::datatype type ::lazy true) [~@args]
                     (list 'delay (list 'new ~name ~@args)))))))

(defn- has-lazy-meta?
    [constructor]
    (cond (symbol? constructor) (::lazy (meta constructor))
          (list? constructor) (recur (first constructor))))
    
(defn- make-constructor
    [type mode constructor]
    (if (or (= mode :lazy) (has-lazy-meta? constructor))
        (make-constructor-lazy type constructor)
        (make-constructor-eager type constructor)))

(defn- lazy-pattern?
    [pattern]
    (cond (symbol? pattern) (::lazy (meta (resolve pattern)))
          (vector? pattern) (recur (constructor-to-factory (first pattern)))))

(defmacro defdatatype
    [type & constructors]
    `(do
         ~@(map (partial make-constructor type :not-lazy) constructors)))

(defmacro deflazy
    [type & constructors]
    `(do
         ~@(map (partial make-constructor type :lazy) constructors)))

(defn- constructor?
    [s]
    (and (symbol? s) (contains? (meta (resolve s)) ::datatype)))
    
(defn- factory-args
    [factory]
    (first (:arglists (meta (resolve factory)))))

(declare transform-condition)

(defn- transform-vector
    [[constructor & params]]
    (let [args  (->> constructor
                     constructor-to-factory
                     factory-args
                     (map keyword))
          pairs (->> params
                     (map transform-condition)
                     (map vector args)
                     (filter (fn [[arg param]] (not= param '_))))]
        (into {} pairs)))

(defn- transform-condition
    [condition]
    (cond (constructor? condition) (constructor-to-keyword condition)
          (vector? condition)      (transform-vector condition)
          :else                    condition))

(defn- transform-row
    [pattern]
    (vec (map transform-condition pattern)))

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
