(ns edu.upc.modelvsdocument.schemas
  (:use [edu.upc.modelvsdocument.utils])
  (:require [schema.core :as s]))

(defn all-of [m] (assoc m s/Keyword s/Any))

(defn Pair 
  ([t1 t2] [(s/one t1 "first")
            (s/one t2 "snd")])
  ([t] (Pair t t)))

; ===================
; FREELING STRUCTURES
; ===================

(def Token 
  "Represents an analyzed word in a sentence"
  (all-of {:pos s/Str
           (s/optional-key :wn) s/Str
           :id  s/Str
           :form s/Str
           }))

(def TokenList [Token])
(def TokenPair [Token]); TODO: Find a better way

(def Argument 
  "An argument for a predicate."
  (all-of {:role s/Str
           :head_token s/Str}))

(def Predicate 
  "A predicate in a sentence. Indicates some action happening"
  (all-of {:id s/Str
           :head_token s/Str
           (s/optional-key :arguments) [Argument]}))

(def Sentence 
  "A sentence."
  (all-of {:tokens [Token]
           (s/optional-key :predicates) [Predicate]}))

(def Paragraph 
  "A paragraph"
  (all-of {:sentences [Sentence]}))

(def Text 
  "An analyzed text, the result of calling the textserver"
  (all-of {:cputime s/Str
           :wordcount s/Str
           :paragraphs [Paragraph]
           :semantic_graph {s/Any s/Any}}))

; ====================
; BPMN MODEL STRUCTURE
; ====================

; Id type defined only as an alias for string
(def Id s/Str)

(def Graph {s/Any #{s/Any}})

(def JBPTProcess org.jbpt.pm.ProcessModel)

(def BPMNProcess 
  {:graph Graph
   :pool (s/maybe org.activiti.bpmn.model.Pool)
   :lanes [org.activiti.bpmn.model.Lane]
   :task-to-lane (s/maybe {s/Str s/Str})
   :process-id s/Str
   :start-event s/Str
   :end-events [s/Str]
   :process org.activiti.bpmn.model.Process})

(def BPMN 
  {:model org.activiti.bpmn.model.BpmnModel
   :processes [BPMNProcess]
   :message-flows [(Pair Id)]})

(def ActivitiModel org.activiti.bpmn.model.BpmnModel)
(def ActivitiProcess org.activiti.bpmn.model.Process)
(def ActivitiTask org.activiti.bpmn.model.Task)
(def ActivitiPool org.activiti.bpmn.model.Pool)
(def ActivitiLane org.activiti.bpmn.model.Lane)

; ==============
;  ORDER MATRIX
; ==============

(def Message (s/pair Id "src" Id "dst"))

;TODO: Move
(defn Vector [inner-schema] 
  (s/both (s/pred vector? "vector")
          [inner-schema]))
(defn RandomAccess [inner-schema]
  (s/both
   (s/pred
    (partial instance? java.util.RandomAccess)
    "random access")
   [inner-schema]))

(def OrderMatrix (Vector (Vector (s/enum :-> :<- :|| :!=))))
(def CostMatrix (Vector (Vector s/Num)))
(def IntCostMatrix (Vector (Vector s/Int)))
