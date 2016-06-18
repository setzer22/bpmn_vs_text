(ns edu.upc.modelvsdocument.extraction.common
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
            [clj-xpath.core :as xpath])
  (:use [com.rpl.specter]
        [clojure.pprint]
        [edu.upc.modelvsdocument.extraction.feature]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils]))


(def pos-filter 
  "POS tags in this list are considered stopwords and are removed from the sentence"
  ["punctuation", "preposition", "determiner", "modal", "conjunction"])

(defn non-stopword? [token :- Token] 
  "Is the token considered a stopword?"
  (not (in? pos-filter (:pos token))))

;; Precompiled paths
(def ALL-SENTENCES 
  "Selects all the sentences in the analyzed text" 
  (comp-paths [:paragraphs ALL :sentences ALL]))

(def TOKENS 
  "Selects all tokens in a sentence" 
  (comp-paths [:tokens ALL non-stopword?]))

(defn verb? [token :- Token] (= "verb" (:pos token)))

(defn token-of-id [s :- Sentence, tk-id]
  (select-one [:tokens ALL #(= tk-id (:id %))] s))

(defn token-list-of-role :- TokenList
      [s :- Sentence, {:keys [from to]}]
  (let [[_, _  , fst] (re-find #"t(\d+)\.(\d+)" from)
        [_, sid, lst] (re-find #"t(\d+)\.(\d+)" to)
        tk-range (map #(str "t" sid "." %) 
                      (range (Integer/parseInt fst) (inc (Integer/parseInt lst))))]
    (map (partial token-of-id s) tk-range)))

(defn token-lemma [s :- Sentence, tk-id]
  (first (select [:tokens ALL #(= tk-id (:id %)) :lemma] s)))

(defn tokens-and-verbs-of-argument-type ;:- ??
      [s :- Sentence, arg-type :- s/Str] 
  (mapcat
    (fn pairs-in-pred [predicate] 
      (let [verb (token-of-id s (:head_token predicate))
            arguments (select [:arguments ALL #(= arg-type (:role %))] predicate)
            tokens-of-argument (map #(filter non-stopword? (token-list-of-role s %)) arguments)]
        (map vector tokens-of-argument (repeat verb))))
    (select [:predicates ALL] s)))

; TODO: Copypasted
(defn heads-and-verbs-of-argument-type 
      [s :- Sentence, arg-type :- s/Str] 
  (mapcat
    (fn pairs-in-pred [predicate] 
      (let [verb (token-of-id s (:head_token predicate))
            head-token-ids (select [:arguments ALL #(= arg-type (:role %)) :head_token] predicate)
            head-tokens (map (partial token-of-id s) head-token-ids)]
        (map vector head-tokens (repeat verb))))
    (select [:predicates ALL] s)))

  


