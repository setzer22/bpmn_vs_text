(ns edu.upc.modelvsdocument.sorter.behavioral-profiles
  (:refer-clojure :exclude [fn defn defrecord])
  (:require [edu.upc.modelvsdocument.bpmn :as bpmn]
            [schema.core :as s :refer [defn fn defrecord]]
            [clojure.set :as set])
  (:use [com.rpl.specter] 
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.bpmn])
  (:import [org.jbpt.pm ProcessModel Activity Gateway AndGateway XorGateway]
           [org.jbpt.pm.bpmn Task]
           [org.jbpt.bp BehaviouralProfile RelSet RelSetType]
           [org.jbpt.bp.construct BPCreatorUnfolding]
           [org.jbpt.petri PetriNet NetSystem]
           [org.jbpt.pm.structure ProcessModel2NetSystem])
  (:gen-class))

; NOTE: We define "tasks" as both tasks and events, because we need both in the sorting relation.
; That's because MessageFlows can go between events and tasks and the MessageFlow algorithm
; wouldn't be able to run otherwise.
; NOTE2: CallActivities and SubProcess are not handled by our algorighm so we ignore them.
(defn gateway? [model :- BPMN, id :- Id] 
  (instance? org.activiti.bpmn.model.Gateway (.getFlowElement (:model model) id)))
(defn task-or-event? [model :- BPMN, id :- Id] 
  (let [flow-elem (.getFlowElement (:model model) id)] 
    (or (instance? org.activiti.bpmn.model.Task flow-elem)
        (instance? org.activiti.bpmn.model.Event flow-elem)))) 

(defn elements-of-process [process :- BPMNProcess]
  (without-repeated (select (walker id?) (:graph process))))

(defn process->jbpt-process [model :- BPMN, process :- BPMNProcess] 
  (let [task-or-event? (partial task-or-event? model)
        [tasks gateways] (span task-or-event? (elements-of-process process))
        id->jbpt-tasks (into {} (map (fn [x] [x (Task. x)]) tasks)) 
        id->jbpt-gateways (into {} (map (fn [x] [x (AndGateway. x)]) gateways))
        id->jbpt-elements (merge id->jbpt-tasks id->jbpt-gateways)
        jbpt-graph (transform (walker id?) id->jbpt-elements (:graph process))
        jbpt-process (ProcessModel.)]
    (doseq [t (vals id->jbpt-tasks)] (.addTask jbpt-process t))
    (doseq [g (vals id->jbpt-gateways)] (.addGateway jbpt-process g))
    (doseq [[src dests] jbpt-graph]
      (doseq [dest dests] (.addControlFlow jbpt-process src dest)))
    {:jbpt-process jbpt-process
     :id->jbpt-elements id->jbpt-elements
     :id->jbpt-tasks id->jbpt-tasks
     :id->jbpt-gateways id->jbpt-gateways}))

(defn map-from-scope [] ())

(def reltype->ordering 
  {RelSetType/Order          :->
   RelSetType/ReverseOrder   :<-
   RelSetType/Exclusive      :!=
   RelSetType/Interleaving   :||})

(defn compute-process-order-relation [model :- BPMN, process :- BPMNProcess]
  (let [{:keys [jbpt-process id->jbpt-elements id->jbpt-tasks]} (process->jbpt-process model process)
        net (ProcessModel2NetSystem/transform jbpt-process)
        bp (.deriveRelationSet (BPCreatorUnfolding/getInstance) net)
        flow-elems (sort-by #(.getName %) (vals id->jbpt-elements))
        n (count flow-elems)]
    (into {} (for [t1 flow-elems]
                [(.getName t1) 
                 (into {} (for [t2 flow-elems, :let [rel (.getRelationForEntities bp t1 t2)]]
                            [(.getName t2) (reltype->ordering rel)]))]))))

(comment 
(def model edu.upc.modelvsdocument.core/model-struct)
(def processes (select [:processes ALL] model))
(def process (second processes))

(let [id->name (into {} (map (fn [id] [id (.getName (.getFlowElement (:model model) id))]) 
                             (elements-of-process process)))
      ]
  (clojure.pprint/pprint id->name))

(print *e)
(def process-order (compute-process-order-relation model process))
(every? not-nil? (for [t (bpmn/all-element-ids model)] 
  (process-order t)))

(process-order "EndEvent_1fx9yp3")

;((process-order "Task_0rpvccw") "Task_0sl26uo")
)

