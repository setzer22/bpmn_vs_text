(ns edu.upc.modelvsdocument.sorter.model-sorter
  (:refer-clojure :exclude [fn defn defrecord])
  (:require [edu.upc.modelvsdocument.bpmn :as bpmn]
            [schema.core :as s :refer [defn fn defrecord]]
            [clojure.set :as set]
            [edu.upc.modelvsdocument.sorter.behavioral-profiles :as bp])
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.schemas]
        [clojure.pprint]
        [edu.upc.modelvsdocument.bpmn])
  (:gen-class))

(comment; REPL
(defn print-for-excel [matrix]
  (doseq [row matrix] 
    (doseq [elem row] (print (str (name elem) " ")))
    (print "\n")))
);END REPL

; TODO: Remove this
;(defn ordering-of-task [task toposort]
  ;"Returns a mapping for each task in the toposort to wether task 
  ;occurs before (:lt), after (:gt) or at the same time (:eq) as it"
  ;(let [task-idx (index-of #(contains? % task) toposort)
        ;prior-tasks (select (walker id?) (select [(srange 0 task-idx) ALL] toposort))
        ;equal-tasks (seq (nth toposort task-idx))
        ;latter-tasks (select (walker id?) (select [(srange (+ 1 task-idx) (count toposort)) ALL] toposort))]
    ;(apply assoc {}
           ;(flatten (concat (zip prior-tasks (repeat :gt))
                            ;(zip equal-tasks (repeat :eq))
                            ;(zip latter-tasks (repeat :lt)))))))


; TODO: Remove this
;(defn initial-order-matrix-old :- OrderMatrix
  ;[model :- BPMN, toposorts :- {Id [#{Id}]}, flow-elems :- [Id]]
  ;(into [] 
        ;(for [i flow-elems]
          ;(let [topo (get toposorts (bpmn/process-id-of-task i model)); The toposort for i's process
                ;ordering (ordering-of-task i topo)]
            ;(mapv
              ;(fn [id] 
                ;(if (contains? ordering id) (ordering id) :un))
              ;flow-elems)))))

(comment ;REPL
  (def model edu.upc.modelvsdocument.core/model-struct)
  (def processes (:processes model))

  (binding [*print-right-margin* 200] 
    (clojure.pprint/pprint 
      ))
)

(defn initial-order-matrix :- OrderMatrix
  ; TODO: Docstring
  ; TODO: Maybe no tasks arg?
  [model :- BPMN, tasks :- [Id]]
  (let [processes (:processes model)
        process-order (apply merge (map (partial bp/compute-process-order-relation model) processes))
        flow-elems (bpmn/all-element-ids model)
        mat-list (for [ti flow-elems, tj flow-elems
                       :let [order ((process-order ti) tj)]]
                   (if order order :!=))]
    (seq-to-matrix mat-list (count flow-elems))))

(defn relative-to :- [s/Int]
  [relation, order-matrix :- OrderMatrix, i :- s/Int]
  "Returns the tasks that come relative to the i-th task and the i-th task itself.
  The ordering is given according to the matrix"
  (let [row (nth order-matrix i)
        task-order (zip (range 0 (count order-matrix)) row)]
    (map first (filter #(= relation (second %)) task-order))))

; TODO: true??
; :gt :<- 
; :lt :->

(defn before [order-matrix i]
  (conj (relative-to :<- order-matrix i) i))

(defn after [order-matrix i]
  (conj (relative-to :-> order-matrix i) i))

(defn adapt-to-message :- OrderMatrix
  [model :- BPMN, order-matrix :- OrderMatrix, flow-elems :- [Id], [src dst] :- Message]
  (let [a (index-of #(= src %) flow-elems)
        b (index-of #(= dst %) flow-elems)
        before-a (before order-matrix a)
        after-b (after order-matrix b)]
    (reduce 
      (fn [matrix [k t]]
        (-> matrix 
            (assoc-in [k t] :->)
            (assoc-in [t k] :<-)))
      order-matrix
      (for [i before-a, j after-b] [i j]))))

(defn order-matrix-with-message-flows
  [model :- BPMN, flow-elems :- [Id], message-flows :- [(Pair Id)]]
  (let [order-matrix (initial-order-matrix model flow-elems) ] 
    ;TODO: This double loop is kind of a mess..
    (loop [order-matrix order-matrix
           old-order-matrix nil 
           num-it 0] 
      (cond 
        ;TODO Prove this always converges so I can remove the exception
        (> num-it 100) (throw (Exception. "Maximum number of iterations for the order matrix."))
        (= old-order-matrix order-matrix) order-matrix
        :else (recur 
                (loop [order-matrix order-matrix
                       [message & remaining-messages] message-flows]
                  (if-not message
                    order-matrix
                    (recur 
                      (adapt-to-message model order-matrix flow-elems message)
                      remaining-messages)))
                order-matrix
                (inc num-it))))))

(defn remove-non-tasks :- OrderMatrix [expanded-matrix :- OrderMatrix, flow-elems :- [Id], model :- BPMN]
  (let [non-task-idxs (into #{} (map first (filter #(not (task? model (second %))) (zip (range) flow-elems))))]
    (mapv (fn [[row _]]
            (mapv first (filterv (fn [[_ idx]] (not (contains? non-task-idxs idx))) (zip row (range))))) 
          (filterv (fn [[row idx]] 
                     (not (contains? non-task-idxs idx))) 
                   (zip expanded-matrix (range))))))

(defn build-order-matrix :- OrderMatrix
  [model :- BPMN]
  (let [;toposorts (into {} (zip (map #(.getId %) (select [:processes ALL :process] model))
        ;                        (map #(topological-sort (:graph (make-acyclic %))) (select [:processes ALL] model))))
        flow-elems (all-element-ids model)
        expanded-matrix (order-matrix-with-message-flows model flow-elems (:message-flows model))
        matrix (remove-non-tasks expanded-matrix flow-elems model)]
    matrix))

;; ----------------
;; Topological sort
;; ----------------

(defn no-incoming-edges [G]
  "Returns the set of G's nodes without incoming edges"
  (set/difference (set (vertices G)) (apply set/union (vals G))))

(defn remove-nodes [G nodes] 
  "The nodes in nodes are removed from the graph. Nodes are string"
    (into {} (for [[v lst] (apply dissoc G nodes)] [v (set/difference lst nodes)])))

(defn topological-sort [G]
  "Returns the topological sorting of G. G must be acyclic"
  (loop [G G, levels []]
    (let [free (no-incoming-edges G)
          G' (remove-nodes G free)] ;in
      (if (empty? free)
        levels
        (recur G' (conj levels free))))))

;; ---------------------
;; Back-edge elimination
;; ---------------------


(defn initial-out [G start] ;;;;
  (assoc 
    ;(apply conj (map #(into {} [[% (into #{} (vertices G))]]) (vertices G))) ; Out [B] = {all nodes} for all B except start
    (into {} 
          (for [v (->> G vertices (remove #(= start %)))] 
            [v (set (vertices G))]))
    start #{start})) ; Out[start] = {start}

(defn next-out [pred out start];;;; 
  "Returns the next out for out given the predecessors in the graph"
  (into {} (map (fn [[v out-v]] 
                  (if (= v start) 
                    [v, out-v]
                    [v, (set/union #{v} (apply set/intersection (map #(out %) (pred v))))]))
                 out)))

(defn dominators [G start max-it]
  "Returns the mapping for every node in the Graph to its set of dominators"
  ; inv: out_(i-1) != out_i, it <= max-it
  (let [pred (reverse-graph G)]; in
    (loop [out_i-1 (initial-out G start),
           out_i (next-out pred out_i-1 start),
           it 0] 
      ;Error condition: Too many iterations, this is not converging.
      (if (> it max-it) (throw (Exception. "Maximum number of iterations for the dominators set reached.")))

      ;Normal loop 
      (if (.equals out_i-1 out_i)
        out_i  
        (recur out_i (next-out pred out_i start) (inc it))))))

(defn back-edges [G start]
  "Returns all the back-edges in G"
  (let [dominators-of (dominators G start 100); TODO: Arbitrary 100
        edge-set (apply concat (map (fn [[v lst]] (zip (repeat v) lst)) G))]
    (filter (fn [[a b]] (contains? (dominators-of a) b)) edge-set)))

(defn remove-edges [G edges] 
  (reduce (fn [G [u v]] (update G u #(disj % v))) G edges))

(defn make-acyclic [{G :graph start :start-event :as process}]
  (update process :graph #(remove-edges % (back-edges G start))))

(defn positions [{G :graph :as process}]
  "Maps each node in G to an integer according to the ordering of the nodes"
  (reduce (fn [G [level nodes]] (apply assoc G (flatten (zip nodes (repeat level))))) 
          {}
          (map-indexed vector (topological-sort (:graph (make-acyclic process))))))

