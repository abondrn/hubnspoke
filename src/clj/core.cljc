;; a set of standalone utility functions
(ns core)

(defn run-on [x & [f]]
  ((or f println) x)
  x)

(defn none-of? [x & things]
  (not (contains? (vec things) x)))

(defn any-of? [x & checks]
  (some #(% x) checks))

(defn all-of? [x & checks]
  (every? #(% x) checks))

(defn sift [p coll & [limit]]
  (loop [remaining coll
         pos []
         neg []
         count 0]
    (if (or (empty? remaining) (= count limit))
      [pos (if (empty? remaining) neg (concat neg remaining))]
      (let [current (first remaining)
            rest (next remaining)]
        (if (p current)
          (recur rest (conj pos current) neg (inc count))
          (recur rest pos (conj neg current) count))))))