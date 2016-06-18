  (ns edu.upc.modelvsdocument.extraction.model-extraction
    (:refer-clojure :exclude [fn defn defrecord])
    (:gen-class)
    (:import [edu.upc.modelvsdocument.extraction.feature Feature])
    (:require [edu.upc.modelvsdocument.extraction.macros :refer [defextractors]]
              [edu.upc.modelvsdocument.bpmn :as bpmn]
              [edu.upc.modelvsdocument.sorter.model-sorter :as modelsorter]
              [clj-http.client :as client]
              [edu.upc.modelvsdocument.textserver :as textserver]
              [clojure.data.json :as json]
              [schema.core :as s :refer [fn defn defrecord]]
              [clj-xpath.core :as xpath]
              [edu.upc.modelvsdocument.wordnet :as wn])
    (:use [com.rpl.specter]
          [edu.upc.modelvsdocument.extraction.feature]
          [edu.upc.modelvsdocument.bpmn]
          [edu.upc.modelvsdocument.extraction.common]
          [edu.upc.modelvsdocument.schemas]
          [edu.upc.modelvsdocument.utils]
          [edu.upc.modelvsdocument.config]))


  (doseq [t [:task-id :pool-id :lane-id]]
    (intern *ns* (symbol (str "sentence-with-" (name t)))
            (fn sentence-with-id [model :- Text, id :- s/Str]
              (select-one [ALL-SENTENCES #(= id (get % t))] model))))

  (defn extract-agents-token-list :- [Feature] [tokens :- TokenList, actions :- TokenList]
    (mapcat (fn [verb] (map #(in-agent [(:lemma %) (:pos %) (:lemma verb)]) tokens)) 
            actions))

  (defn extract-agents-pool :- [Feature] 
        [task-id :- s/Str, pool-id :- s/Str model-an :- Text, actions :- TokenList]
    (let [tokens-pool (select [ALL-SENTENCES #(= pool-id (:pool-id %)) TOKENS] model-an)]
      (when-not (nil? pool-id) (extract-agents-token-list tokens-pool actions))))

  (defn extract-agents-lane :- [Feature] 
        [task-id :- s/Str, lane-id :- s/Str, model-an :- Text, actions :- TokenList]
    (let [tokens-lane (select [ALL-SENTENCES #(= lane-id (:lane-id %)) TOKENS] model-an)]
      (when-not (nil? lane-id) (extract-agents-token-list tokens-lane actions))))


  ; TODO: I'll leave this undone because I've yet to find a task text that
  ;       contains an agent.
  (defn extract-agents-task :- [Feature] 
        [task-id :- s/Str, model-an :- Text]
    nil)

  (defextractors [task-id, model :- BPMN, analyzed-model :- Text]
    (extract-synsets
      (let [analyzed-text (select 
                            [ALL-SENTENCES #(= task-id (:task-id %)) TOKENS :wn] 
                            analyzed-model)]
        (map #(has-synset [%]) analyzed-text)))

    (extract-synset-parents
      (let [synsets (select 
                      [ALL-SENTENCES #(= task-id (:task-id %)) TOKENS :wn] 
                      analyzed-model)]
        (mapcat (fn [chain-with-indexes] 
                  (remove nil? 
                          (map (fn [[syn idx]] (when-not (nil? syn) 
                                                 (has-parent-synset 
                                                   [syn] 
                                                   (Math/pow (config :hiperonimy-multiplier) idx)))) 
                               chain-with-indexes)))
                (remove #(nil? (first %)) 
                        (map 
                          #(take (config :hiperonimy-chain-length)
                                 (zip (wn/hiperonimy-chain-of %) (range))) 
                          (remove nil? synsets))))))

    (extract-lemmas
      (let [analyzed-text (select 
                            [ALL-SENTENCES #(= task-id (:task-id %)) TOKENS] 
                            analyzed-model)]
        (map #(has-lemma [(:pos %) (:lemma %)]) analyzed-text)))

    (extract-actions 
      (let [analyzed-text (select 
                            [ALL-SENTENCES #(= task-id (:task-id %)) TOKENS #(= (:pos %) "verb")] 
                            analyzed-model)]
        (map #(has-action [(:lemma %)]) analyzed-text)))

    ; Note: 
    ; If we find multiple verbs in the same task we associate each agent
    ; found on the pool/lanes to all of the verbs, because it's implied 
    ; that agent is performing all of the actions in the task text.
    ; TODO: Not really true. Indirect actions contained in nouns like 
    ;       "shipment" don't follow this rule and create false features.
    (extract-agents 
      (let [task-sentence (sentence-with-task-id analyzed-model task-id)
            lane-id (let [t (bpmn/lane-of-task task-id model)] (when-not (nil? t) (.getId t)))
            pool-id (let [p (bpmn/pool-of-task task-id model)] (when-not (nil? p) (.getId p)))
            actions (map (partial token-of-id task-sentence) 
                         (select [:predicates ALL :head_token] task-sentence))]
        (concat 
          (when-not (nil? pool-id) 
            (extract-agents-pool task-id pool-id analyzed-model actions)) ; Agents in pool text
          (when-not (nil? lane-id) 
            (extract-agents-lane task-id lane-id analyzed-model actions)) ; Agents in lane text
          (extract-agents-task task-id analyzed-model))))              ; Agents in task text


    (extract-patients
      (let [s (sentence-with-task-id analyzed-model task-id)]
        (concat
          ; patient-head means "the head token of the agent is..."
          (map (fn head-feature-list [[tk v]] 
                 (patient-head [(:lemma tk) (:pos tk) (:lemma v)])) 
               (heads-and-verbs-of-argument-type s "A1"))
          ; in-agent means "the agent contains token..."
          (mapcat (fn feature-list 
                    [[tks v]] (map #(in-patient [(:lemma %1) (:pos %1) (:lemma %2)]) tks (repeat v))) 
                  (tokens-and-verbs-of-argument-type s "A1")))))
    
    (extract-conditionals 
      (let [process (process-of-task task-id model)
            named-gateway? (set (remove nil? (select [ALL-SENTENCES :gateway-id] analyzed-model)))
            successors (filter 
                         named-gateway?
                         (remove nil? 
                                 ((process :graph) task-id)))
            predecessors (filter named-gateway?
                                 (remove nil? ((reverse-graph (process :graph)) task-id)))
            predecessor-tokens (mapcat (fn [g-id] 
                                         (select [ALL-SENTENCES #(= g-id (:gateway-id %)) TOKENS] analyzed-model)) 
                                       predecessors)
            successor-tokens (mapcat (fn [g-id] 
                                     (select [ALL-SENTENCES #(= g-id (:gateway-id %)) TOKENS] analyzed-model)) 
                                   successors)
          ] 
      (concat 
        (map (fn [t :- Token] (lemma-conditional-pred [(:lemma t) (:pos t)])) 
             predecessor-tokens)
        (comment (map (fn [t :- Token] (lemma-conditional-pred [(:lemma t) (:pos t)])) 
             successor-tokens)))))
  )

;(clojure.pprint/pprint (map #(extract-patients (.getId %) model-struct model-an) (bpmn/all-tasks model-struct)))

; DEFS FOR TESTING
;(def model-struct edu.upc.modelvsdocument.core/model-struct)
;(def model-an edu.upc.modelvsdocument.core/model-an)
;(def -sentences (select [:paragraphs ALL :sentences ALL] model-an))

;TODO: Make this into a unit test...
;(s/validate
  ;{ActivitiTask ActivitiPool}
  ;(into {} (map #(vector % (bpmn/pool-of-task (.getId %) model-struct)) (bpmn/all-tasks model-struct))))

; TODO: This too
;(doseq [task-id (map #(.getId %) (bpmn/all-tasks model-struct))] (s/validate [Feature] (extract-agents task-id model-struct model-an)))
