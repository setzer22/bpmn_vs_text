(ns edu.upc.modelvsdocument.extraction.text-extraction
  (:refer-clojure :exclude [fn defn defrecord])
  (:gen-class)
  (:require [schema.core :as s :refer [defn defrecord fn]]
            [clj-http.client :as client]
            [edu.upc.modelvsdocument.textserver :as textserver]
            [edu.upc.modelvsdocument.extraction.feature]
            [edu.upc.modelvsdocument.extraction.constituents-matching :as constituents]
            [clojure.data.json :as json]
            [clj-xpath.core :as xpath]
            [incanter.core :as mth]
            [edu.upc.modelvsdocument.wordnet :as wn])
  (:import [edu.upc.modelvsdocument.extraction.feature Feature])
  (:use [edu.upc.modelvsdocument.schemas]
        [com.rpl.specter] 
        [edu.upc.modelvsdocument.extraction.common]
        [edu.upc.modelvsdocument.extraction.feature]
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.config]))

; TODO LIST:
;   - POS filter not working properly
;   - Detect references in-sentence
;   - Detect co-references accross sentences
;   - Investigate what happens with verbs like hand over

(defn extract-lemmas [s :- Sentence]
  "Extracts the lemmas of a sentence"
  (map #(has-lemma [(:pos %) (:lemma %)]) (select [TOKENS] s)))

(defn extract-synsets [s :- Sentence]
  (map #(has-synset [%]) (remove nil? (select [TOKENS :wn] s))))

(defn extract-synset-parents [s :- Sentence]
  (mapcat (fn [chain-with-indexes] 
            (remove nil? 
                    (map (fn [[syn idx]] (when-not (nil? syn) 
                                           (has-parent-synset 
                                             [syn] 
                                             (Math/pow (config :hiperonimy-multiplier) idx)))) 
                         chain-with-indexes)))
          (map 
            #(take (config :hiperonimy-chain-length) 
                   (zip (wn/hiperonimy-chain-of %) (range))) 
            (remove nil? (select [TOKENS :wn] s)))))

(defn extract-conditionals [s :- Sentence]
  (let [conditional-tokens (filter non-stopword? (constituents/extract-tokens-in-condition s))]
    (map #(lemma-conditional-follow [(:lemma %) (:pos %)]) conditional-tokens)))

;TODO Filter auxiliary verbs
(defn extract-actions [s :- Sentence]
  (map
    (fn [tk-id] (has-action [(token-lemma s tk-id)]))
    (select [:predicates ALL :head_token] s)))

(defn extract-agents :- [Feature] 
      [s :- Sentence]
  ; We create two kinds of agent-features
  (concat
    ; agent-head means "the head token of the agent is..."
    (map (fn head-feature-list [[tk v]] 
           (agent-head [(:lemma tk) (:pos tk) (:lemma v)])) 
         (heads-and-verbs-of-argument-type s "A0"))
    ; in-agent means "the agent contains token..."
    (mapcat (fn feature-list 
              [[tks v]] (map #(in-agent [(:lemma %1) (:pos %1) (:lemma %2)]) tks (repeat v))) 
            (tokens-and-verbs-of-argument-type s "A0"))))

(defn extract-patients :- [Feature]
      [s :- Sentence]
  ; TODO: Copypasted // changed in-agent -> in-patient. Common pattern here...
  (concat
    ; agent-head means "the head token of the agent is..."
    (map (fn head-feature-list [[tk v]] 
           (patient-head [(:lemma tk) (:pos tk) (:lemma v)])) 
         (heads-and-verbs-of-argument-type s "A1"))
    ; in-agent means "the agent contains token..."
    (mapcat (fn feature-list 
              [[tks v]] (map #(in-patient [(:lemma %1) (:pos %1) (:lemma %2)]) tks (repeat v))) 
            (tokens-and-verbs-of-argument-type s "A1"))))

(def feature-extractors [extract-synsets extract-synset-parents extract-conditionals extract-lemmas extract-actions extract-agents extract-patients])

(defn extract-features [text :- Text] 
  "Extracts all features from sentences in the text"
  (map 
    (fn [s :- Sentence] (flatten (map #(% s) feature-extractors)))
    (select [ALL-SENTENCES] text)))

;(def text (textserver/textserver->json 
            ;(textserver/analyze-cached 
              ;(slurp "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Dispatch-of-goods.txt") 
              ;"en")))
;(def -sentences (select [:paragraphs ALL :sentences ALL] text))
