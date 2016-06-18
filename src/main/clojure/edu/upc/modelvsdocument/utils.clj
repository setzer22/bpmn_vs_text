(ns edu.upc.modelvsdocument.utils
  (:gen-class)
  (:require [clojure.set :as set]))

(defn grep-methods 
  ([clss re]
   (filter #(re-matches re %) (map #(.getName %) (.getMethods clss))))
  ([clss] 
   (grep-methods clss #".*")))

; Deprecated by spyscope, which is both way more useful and precise
(defmacro plet [bindings & body]
  "Same as let but prints each binding. Used for debug"
  (let
    [aux-symbol (gensym)]
    (concat 
      (list 
        'let
        (into [] (apply concat (map 
                                 (fn [[id value]] [id value, aux-symbol (list 'println (list 'str "[PLET] " (name id) " = " value))]) 
                                 (partition 2 bindings)))))
      body)))

(defmacro comp-if [expr neg zero pos]
  "Returns neg, zero or pos depending on the numerical value of expr"
  `(let [res# ~expr] 
     (cond (< res# 0) ~neg
           (= res# 0) ~zero 
           :else       ~pos)))


;; NOTE: sortest and longest have "complementary" default behaviour so you can 
;;       bind two alias names to both the shortest and longest of two sequences
;;       like so: 
;;         (let [long (longest c1 c2)
;;               short (shortest c1 c2)])
(defn shortest [coll1 coll2]
  "Returns the shortest collection. When equal size coll1 is returned"
  (if (<= (count coll1) (count coll2)) coll1 coll2))

(defn longest [coll1 coll2]
  "Returns the longest collection. When equal size coll2 is returned"
  (if (> (count coll1) (count coll2)) coll1 coll2))

(defn approx-eq [epsilon x y] 
  (< (- x y) epsilon))

(def approx? (partial approx-eq 0.00001))

(defn map-by [lst f]
  "lst is a list of maps, returns a map with lst's elements indexed by their k key"
  (reduce 
    (fn [new-map e] (assoc new-map (f e) e))
    {}
    lst))

;WIL: Gonna add an exception to this because its failing 
(defn index-of [pred lst] 
  (let [index (count (take-while (complement pred) lst))]
    (if (= index (count lst))
      (throw (Exception. (str "No element in " lst " is true for predicate " pred)))
      index)))

(defn in? [seq elm]  
  "True if seq contains elm"
  (if (some #(= elm %) seq) true false))

(defn zip [L1 L2]
  (map vector L1 L2))

(defn nth2 [coll i j]
  "Coll is a list of lists. Returns the element (i,j) of coll" 
  (nth (nth coll i) j))

; Alias for map to better express the intent of zipping
(def zip-with map)

(def not-nil? (comp not nil?))

(defn seq-intersection [fv1, fv2]
  (seq (set/intersection (into (sorted-set) fv1) (into (sorted-set) fv2))))

(defn seq-difference [fv1, fv2]
  (seq (set/difference (into (sorted-set) fv1) (into (sorted-set) fv2))))

(defn get-matrix [mat i j]
  (let [N (count mat)
        M (count (nth mat 0))]
    (if (or (>= i N) (>= j M))
      (throw (Exception. (str "Out of bounds access for indices ["i "," j "].")))
      (get-in mat [i j]))))

(defn remove-nil [lst] (remove nil? lst))

(defn without-repeated [lst]
  (seq (into (sorted-set) lst)))

(defn span [pred lst] (reduce 
                        (fn [[Y N] x] (if (pred x) 
                                        [(concat Y [x]) N]
                                        [Y (concat N [x])]))
                        [[] []] 
                        lst))

(defn seq-to-matrix [lst n]
  "Converts lst into a matrix with columns of size n"
  (into [] (map #(into [] %) (partition n lst))))

(defmacro square [x] (list * x x))
