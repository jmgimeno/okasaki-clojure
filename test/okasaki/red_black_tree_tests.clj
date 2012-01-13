(ns okasaki.red-black-tree-tests
    (:use okasaki.red-black-tree
          datatype.core
          clojure.test))

(deftest tree-without-nodes
    (is (not (member 1 Empty))))

(deftest one-node-tree
    (is (member 1 (insert 1 Empty)))
    (is (not (member 1 (insert 2 Empty))))
    (is (not (member 2 (insert 1 Empty)))))

(deftest two-nodes-tree
    (is (member 1 (insert 1 (insert 2 Empty))))
    (is (member 1 (insert 2 (insert 1 Empty))))
    (is (not (member 3 (insert 1 (insert 2 Empty)))))
    (is (not (member 0 (insert 1 (insert 2 Empty))))))    

(deftest many-nodes-tree
    (let [elems (range 100)
          rbt (reduce #(insert %2 %1) Empty elems)]
        (is (member 50 rbt))
        (is (not (member 1000 rbt)))))

(defun all-color-paths
    ;;[tree]
    [Empty] [[]]
    [[Node c l x r]] (let [ps (into (all-color-paths l) (all-color-paths r))]
                         (map #(conj % c) ps)))

(deftest no-red-node-has-a-red-child
    (is (->> (range 100)
             (reduce #(insert %2 %1) Empty)
             all-color-paths
             (mapcat (partial partition 2 1))
             (not-any? (fn [[c p]] (and (= :okasaki.red-black-tree/Red p)
                                       (= :okasaki.red-black-tree/Red c)))))))

(deftest same-number-of-black-nodes-on-every-path
    (is (= 1 (->> (range 100)
                  (reduce #(insert %2 %1) Empty)
                  all-color-paths
                  (map (partial filter #(= :okasaki.red-black-tree/Black %)))
                  (map count)
                  set
                  count))))

