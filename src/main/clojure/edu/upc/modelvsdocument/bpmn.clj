(ns edu.upc.modelvsdocument.bpmn
  (:refer-clojure :exclude [fn defn defrecord])
  (:gen-class)
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils])
  (:require [schema.core :as s :refer [fn defn defrecord]]
            [spyscope.core :as spy]
            [clojure.set :as set])
  (:import [edu.upc.modelvsdocument.ModelManager]
           [org.activiti.bpmn.model Task StartEvent EndEvent UserTask SequenceFlow BpmnModel]))

; Returns wether an object represents a bpmn identifier
(def id? string?)

; Some utility methods to access BPMN sturctures
(defn all-tasks [{model :model} :- BPMN]
  (sort-by
    #(.getId %)
    (filter 
      #(instance? Task %)
      (flatten (map 
                 #(seq (.getFlowElements %))
                 (.getProcesses model))))))

(defn all-gateways [{model :model} :- BPMN]
  (sort-by
    #(.getId %)
    (filter 
      #(instance? org.activiti.bpmn.model.Gateway %)
      (flatten (map 
                 #(seq (.getFlowElements %))
                 (.getProcesses model))))))

(defn all-gateway-ids [model :- BPMN]
  (map #(.getId %) (all-gateways model)))

(defn all-task-ids [model :- BPMN]
  (map #(.getId %) (all-tasks model)))

(defn all-sequenceflows [model :- BPMN]
  (mapcat (fn [process :- ActivitiProcess] 
            (filter #(instance? SequenceFlow %) (.getFlowElements process)))
          (select [:processes ALL :process] model)))

(defn all-element-ids [model :- BPMN]
  (sort
    (without-repeated 
      (mapcat (fn [seqflow] [(.getSourceRef seqflow) (.getTargetRef seqflow)]) (all-sequenceflows model)))))

(defn all-lanes [model :- BPMN]
  (select [:processes ALL :lanes ALL] model))

(defn all-pools [model :- BPMN]
  (select [:processes ALL :pool not-nil?] model))

(defn task? [model :- BPMN, task-id :- Id]
  (instance? org.activiti.bpmn.model.Task (.getFlowElement (:model model) task-id)))

(defn exclusive-gateway? [model :- BPMN, task-id :- Id] 
  (instance? org.activiti.bpmn.model.ExclusiveGateway (.getFlowElement (:model model) task-id)))

(defn parallel-gateway? [model :- BPMN, task-id :- Id] 
  (instance? org.activiti.bpmn.model.ParallelGateway (.getFlowElement (:model model) task-id)))

(defn inclusive-gateway? [model :- BPMN, task-id :- Id] 
  (instance? org.activiti.bpmn.model.InclusiveGateway (.getFlowElement (:model model) task-id)))

(defn process-of-task [task-id :- s/Str, model :- BPMN]
  (select-one [:processes ALL (selected? [:process #(.getFlowElement % task-id)])] model))

(defn map-all-to-k [k vals]
  (reduce #(assoc %1 %2 k) {} vals))

(defn conj-into [G edges]
  "Adds all edges in edges to g"
  (reduce (fn [G' [u v]] (assoc G' u (conj (get G' u #{}) v)))  G  edges))

(defn reverse-graph [G]
  (reduce (fn [G [v adj]] (conj-into G (map-all-to-k v adj)))
          {}
          G )) 

(defn process-id-of-task [task-id :- s/Str, model :- BPMN]
  (.getId (:process (process-of-task task-id model))))

(defn pool-of-task [task-id :- s/Str, model :- BPMN]
  (:pool (process-of-task task-id model)))

(defn lane-of-task [task-id :- s/Str, model :- BPMN]
  (let [process (process-of-task task-id model)] 
    (when-not (nil? (:task-to-lane process)) 
      (.getLane (:model model) (get (:task-to-lane process) task-id)))))

; Parsing and creating BPMN models
(declare build-graph)
(declare build-process)
(declare task-to-lane)

(defn flow-element? [model id] (not (nil? (.getFlowElement model id))))

(defn read-model [path]
  "Reads the BPMN model in path"
  (edu.upc.modelvsdocument.ModelManager/readModel path))

(defn get-message-flows [model :- ActivitiModel] :- [(Pair Id)]
  (as-> (seq (.values (.getMessageFlows model))) messages 
    (map (fn [m] [(.getSourceRef m) (.getTargetRef m)]) messages)
    ; We remove those message flows that go into anything that's not
    ; a flow element (i.e. a Pool) because they're not interesting to us
    (filter 
      (fn [[src, dst]] (and (flow-element? model src)
                            (flow-element? model dst)))
      messages)
    ; We also remove flow messages that come from, or go to the same task 
    ; because that makes it impossible to sort tasks.
    (first (reduce 
             (fn [[ok-messages, used] [src, dst :as message]] 
               [(if (or (contains? used dst) (contains? used src)) 
                  ok-messages
                  (conj ok-messages message)), 
                (set/union used #{src dst})])
             [[], #{}]
             messages))))

(defn build-model [model :- ActivitiModel] :- BPMN
  "Builds a BPMN model as a collection of processes."
  {:model  model

   :processes 
   (->> (.getProcesses model) 
        (map (partial build-process model)) 
        (remove nil?)
        (into []))

   :message-flows 
   (get-message-flows model)})

(defn build-process [model :- ActivitiModel, process :- ActivitiProcess] 
  "Builds a process structure suitable for analyzing the process in the BPMN model"
  (let [;NOTE: There is only one start event, if that is 
        ;      not the case we fail
        start-node (let [start-nodes (filter #(instance? StartEvent %) (.getFlowElements process))]
                     (cond 
                       (> (count start-nodes) 1) (throw (Exception. "A model can't have more than one start task"))
                       (< (count start-nodes) 1) nil ; This makes the function return null
                       :else                     (first start-nodes)))
        end-nodes (filter #(instance? EndEvent %) (.getFlowElements process))
        sequenceflow-nodes (filter #(instance? SequenceFlow %) (.getFlowElements process))] 
    (when (not (nil? start-node)) 
      {:graph (apply assoc 
                     (build-graph sequenceflow-nodes)
                     (flatten (map (fn [e] [(.getId e) #{}]) end-nodes))) 
       :pool (first (filter #(= (.getProcessRef %) (.getId process)) (.getPools model)))
       :lanes (.getLanes process)
       :task-to-lane (task-to-lane process)
       :process-id (.getId process)
       :start-event (.getId start-node)
       :end-events (map #(.getId %) end-nodes)
       :process process})))

(defn task-to-lane [process :- ActivitiProcess]
  (when (not (empty? (.getLanes process)))
    (let [lane-ids (map #(.getId %) (.getLanes process))
          nodes-by-lane (map #(.getFlowReferences %) (.getLanes process))]
      (apply merge (map
               (fn [id nodes] (into {} (map vector nodes (repeat id))))
               lane-ids
               nodes-by-lane)))))

(defn build-graph [sequenceflow-nodes :- [SequenceFlow]] 
  "Given a list of SequenceFlow nodes, returns a graph as an adjacency list using 
  sets as the list with the nodes stored as the string ids in the BPMN model."
  (reduce 
        (fn [G, e :- SequenceFlow]
          (let [src (.getSourceRef e), dst (.getTargetRef e)] ; in
            (update G src #(if (nil? %) #{dst} (conj % dst)))))
        {}
        sequenceflow-nodes))

(defn vertices [G]
  "The vertex set of G"
  (keys G))

;(def m (read-model "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Credit-scoring.bpmn"))
;(def mdl (build-model m))
;(binding [*print-level* 5] (clojure.pprint/pprint mdl))

