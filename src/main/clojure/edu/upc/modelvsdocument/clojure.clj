(ns edu.upc.modelvsdocument.clojure
  (:gen-class)
  (:require [clojure.set :as set])
  (:import (org.activiti.bpmn.model StartEvent EndEvent UserTask SequenceFlow BpmnModel)))

; The BPMN model path
(def path "/home/josep/BPMN/model.xml")

(declare build-graph)

(defn read-model [path]
  "Reads the BPMN model in path"
  (edu.upc.modelvsdocument.ModelManager/readModel path))

(defn build-graph-from-model [model] 
  "Transforms the BPMN model given to a graph represented as adjacency lists with sets."
  (let [process (.get (.getProcesses model) 0),
        start-nodes (filter #(instance? StartEvent %) (.getFlowElements process)),
        end-nodes (filter #(instance? EndEvent %) (.getFlowElements process)),
        usertask-nodes (filter #(instance? UserTask %) (.getFlowElements process)),
        sequenceflow-nodes (filter #(instance? SequenceFlow %) (.getFlowElements process))
       ]; in 
          (build-graph sequenceflow-nodes)))

(defn build-graph [sequenceflow-nodes] 
  "Given a list of SequenceFlow nodes, returns a graph as an adjacency list using 
  sets as the list with the nodes stored as the string ids in the BPMN model."
  (reduce 
    (fn [G ^SequenceFlow e]
      (let [src (.getSourceRef e), dst (.getTargetRef e)] ; in
        (update G src #(if (nil? %) #{dst} (conj % dst)))))
        ;(assoc G 
               ;src 
               ;(conj (get G src #{}) (.getTargetRef e)))))
    {}
    sequenceflow-nodes))

(defn vertices [G]
  "The vertex set of G"
  (keys G))

(defn get-gateways [G] 
  "Returns all the gateway nodes in G" 
  (filter #(re-find #"^.*gateway[0-9]+$" %) (vertices G)))

(defn get-start 
  "Returns the start node of G. G must be a flow graph." 
  [G] "startevent1")
  ;[G] 1)


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

(defn initial-out [G]
  (assoc 
    (apply conj (map #(into {} [[% (into #{} (vertices G))]]) (vertices G))) ; Out [B] = {all nodes} for all actions except Entry
    (get-start G) #{(get-start G)})) ; Out[Entry] = {Entry}

(defn reverse-map [k vals]
  (reduce #(assoc %1 %2 k) {} vals))

(defn conj-into [G edges]
  "Adds all edges in edges to g"
  (reduce (fn [G' [u v]] (assoc G' u (conj (get G' u #{}) v)))  G  edges))

(defn reverse-graph [G]
  (reduce (fn [G [v adj]] (conj-into G (reverse-map v adj)))
          {}
          G )) 

(defn next-out [pred out]
  "Returns the next out for out given the predecessors in the graph"
  (into {} (map (fn [[v out-v]] 
                  (if ( = v (get-start pred)) 
                    [v out-v]
                    [v, (set/union #{v} (apply set/intersection (map #(out %) (pred v))))]))
                 out)))

(defn dominators [G max-it]
  "Returns the mapping for every node in the Graph to its set of dominators"
  ; inv: out_(i-1) != out_i, it <= max-it
  (let [pred (reverse-graph G)]; in
    (loop [out_i-1 (initial-out G),
           out_i (next-out pred out_i-1),
           it 0] 
      (if (or (.equals out_i-1 out_i) (> it max-it))
        out_i  ; Algorithm has converged
        (recur out_i (next-out pred out_i) (inc it))))))  ; Keep iterating


(defn zip [L1 L2]
  (map vector L1 L2))

(defn back-edges [G]
  "Returns all the back-edges in G"
  (let [dominators-of (dominators G 100); TODO: Arbitrary 100
        edge-set (apply concat (map (fn [[v lst]] (zip (repeat v) lst)) G))]
    (filter (fn [[a b]] (contains? (dominators-of a) b)) edge-set)))

(defn remove-edges [G edges] 
  (reduce (fn [G [u v]] (update G u #(disj % v))) G edges))

(defn make-acyclic [G]
  (remove-edges G (back-edges G)))

(defn positions [G]
  "Maps each node in G to an integer according to the ordering of the nodes"
  (reduce (fn [G [level nodes]] (apply assoc G (flatten (zip nodes (repeat level))))) 
          {}
          (map-indexed vector (-> G make-acyclic topological-sort))))

(defn do-stuff []
  "Does stuff"
  (->> path read-model build-graph-from-model topological-sort .toString println))

;; DEMO: Making a graph acyclic
;(def G {1 #{2 3} 2 #{4} 3 #{4} 4 #{1 5} 5 #{1}})
;(identity G)
;(dominators G 100)
;(make-acyclic G)

;; DEMO: Reading a BPMN model as a graph
;; (remember to adjust get-start)
;(def G2 (build-graph-from-model (read-model path)))
;(identity G2)
;(back-edges G2)
;(make-acyclic G2)
;(topological-sort G2)
