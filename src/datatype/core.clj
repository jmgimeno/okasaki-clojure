(ns datatype.core
    (:use [clojure.core.match :only [match]]
          [clojure.string :only [join lower-case split]]))

(defn- constant-value
    [s]
    (keyword (str *ns*) (str s)))

(defn- factory-symbol
    [s]
    (let [parts (split (str s) #"[.]")
          ns (join "." (butlast parts))
          factory (str "->" (last parts))]
        (symbol (if (empty? ns)
                    factory
                    (str ns "/" factory)))))

(defn- mangle-record-fields
    [name args]
    (map #(symbol (lower-case (str name "-" %))) args))

(defn- make-constructor-strict
    [type constructor]
    (if (symbol? constructor)
        ;; constant constructor
        `(def ~(vary-meta  constructor assoc ::datatype type)
             ~(constant-value constructor))
        ;; factory constructor
        (let [[name & args] constructor
              margs (mangle-record-fields name args)]
            `(do
                 (defrecord ~name [~@margs])
                 (alter-meta! (var ~(factory-symbol name)) assoc ::datatype ~type)))))

(defn- make-constructor-lazy
    [type constructor]
    (if (symbol? constructor)
        ;; constant constructor
        `(def ~(vary-meta  constructor assoc ::datatype type ::lazy true)
             (delay ~(constant-value constructor)))
        ;; factory constructor
        (let [[name & args] constructor
              margs (mangle-record-fields name args)]
            `(do
                 (defrecord ~name [~@margs])
                 (defmacro ~(vary-meta (factory-symbol name) assoc ::datatype type ::lazy true) [~@margs]
                     (list 'delay (list 'new ~name ~@margs)))))))

(defn- has-lazy-meta?
    [constructor]
    (cond (symbol? constructor) (::lazy (meta constructor))
          (list? constructor) (recur (first constructor))))
    
(defn- make-constructor
    [type mode constructor]
    (if (or (= mode :lazy) (has-lazy-meta? constructor))
        (make-constructor-lazy type constructor)
        (make-constructor-strict type constructor)))

(defmacro defdatatype
    [type & constructors]
    `(do
         ~@(map (partial make-constructor type :strict) constructors)))

(defmacro deflazy
    [type & constructors]
    `(do
         ~@(map (partial make-constructor type :lazy) constructors)))

(defn- constructor?
    [symbol]
    (contains? (meta (resolve symbol)) ::datatype))

(defn- lazy?
    [symbol]
    (contains? (meta (resolve symbol)) ::lazy))

(defn- constant?
    [condition]
    (and (symbol? condition) (constructor? condition)))

(defn- lazy-constant?
    [condition]
    (and (constant? condition) (lazy? condition)))

(defn- factory?
    [condition]
    (and (vector? condition)
         (let [[constructor & _] condition
               factory (factory-symbol constructor)]
             (constructor? factory))))

(defn- lazy-factory?
    [condition]
    (and (factory? condition)
         (let [[constructor & _] condition
               factory (factory-symbol constructor)]
             (lazy? factory))))

(defn- lazy-condition?
    [condition]
    (or (lazy-constant? condition) (lazy-factory? condition)))

(defn- factory-args
    [factory]
    (first (:arglists (meta (resolve factory)))))

(defn- filter-rows
    [rows]
    (filter #(not= :else %) rows))

(defn- transform-constant
    [constant]
    (keyword (subs (str (resolve constant)) 2)))

(declare transform-condition)

(defn- transform-factory
    [[constructor & params]]
    (let [args  (->> constructor
                     factory-symbol
                     factory-args
                     (map keyword))
          pairs (->> params
                     (map transform-condition)
                     (map vector args)
                     (filter (fn [[_ param]] (not= param '_))))]
        (into {} pairs)))

(defn- transform-condition
    [condition]
    (cond (constant? condition) (transform-constant condition)
          (factory? condition)  (transform-factory condition)
          :else                 condition))

(defn- transform-row
    [row]
    (if (= row :else)
        row
        (vec (map transform-condition row))))

(defn- transform-arg
    [arg needs-force]
    (if needs-force
        `(force ~arg)
        arg))

(defmacro caseof
    [args & rules]
    (let [row-action-pairs  (partition 2 rules)
          rows              (map first row-action-pairs)
          actions           (map second row-action-pairs)
          filtered-rows     (filter-rows rows)
          transposed-rows   (apply map vector filtered-rows)
          need-force        (map #(some lazy-condition? %) transposed-rows)
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

