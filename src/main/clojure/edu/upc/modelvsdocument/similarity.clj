(ns edu.upc.modelvsdocument.similarity
  (:refer-clojure :exclude [fn defn defrecord])
  (:require [schema.core :as s :refer [fn defn defrecord]]
            [edu.upc.modelvsdocument.extraction.text-extraction :as txt]
            [incanter.core :as mth]
            [clojure.set :as set])
  (:use [clojure.pprint]
        [edu.upc.modelvsdocument.extraction.feature]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.config])
  (:import [edu.upc.modelvsdocument.extraction.feature Feature])
  (:gen-class))

(defn magnitude [v :- [Feature]]
  "Computes the magnitude for the given feature vector"
  (Math/sqrt (reduce (fn [acc, {w :weight}] (+ acc (* w w))) 0 v)))

(defn normalize [v :- [Feature]]
  "Normalizes the given feature vector"
  (let [len (magnitude v)] 
    (map (fn [{w :weight :as feature}] (assoc feature :weight (/ w len))) v)))

(do  
  (defn cosine-similarity [v1 :- [Feature], v2 :- [Feature]]
    "Computes the cosine similarity between two feature vectors"
    (loop [[{x-weight :weight :as x} & xs :as fv1] (normalize (sort v1)), 
           [{y-weight :weight :as y} & ys :as fv2] (normalize (sort v2)), 
           sum 0]
      (if (or (nil? x) (nil? y))
        (if (> sum 1) 1 sum) ; Result might be > than 1 because FP precision error
        (comp-if (compare x y)
                 (recur xs fv2 sum)
                 (recur xs ys (+ sum (* x-weight y-weight)))
                 (recur fv1 ys sum)))))

  (defn jaccard-similarity [v1 :- [Feature], v2 :- [Feature]]
    (let [s1 (set v1)
          s2 (set v2)]
      (/ (count (set/intersection s1 s2))
         (count (set/union s1 s2)))))

  (defn fake-jaccard [v1 :- [Feature], v2 :- [Feature]]
    (let [sh (shortest v1 v2)
          ln (longest v1 v2)
          short-set (set sh)
          long-set (set ln)]
      (/ (count (set/intersection short-set long-set))
         (count short-set))))

  (defn weighted-fake-jaccard [v1 :- [Feature], v2 :- [Feature]]
    (let [sh (shortest v1 v2)
          ln (longest v1 v2)
          short-set (set sh)
          long-set (set ln)
          S (reduce + (map :weight short-set))
          I (reduce + (map :weight (set/intersection short-set long-set)))]
      (/ I S)))

  (defn weighted-jaccard-similarity [v1 :- [Feature], v2 :- [Feature]]
    (let [s1 (set v1)
          s2 (set v2)
          U (reduce + (map :weight (set/union s1 s2)))
          I (reduce + (map :weight (set/intersection s1 s2)))]
      (/ I U)))

  (def similarity-functions
    {:jaccard jaccard-similarity
     :fake-jaccard fake-jaccard
     :weighted-fake-jaccard weighted-fake-jaccard
     :weighted-jaccard weighted-jaccard-similarity
     :cosine cosine-similarity})
  
  (defn default-similarity [v1, v2]
    (((keyword (:similarity-function config)) similarity-functions) v1 v2)))

(defn similarity-matrix-threshold [sim-matrix :- CostMatrix]
  (let [data (remove zero? (map double (flatten sim-matrix)))
        N (count data)
        mean (double (/ (reduce + data) N))
        ;sd (Math/sqrt (double (/ (reduce + (map #(square (- % mean)) data)) N)))
        mad (/ (reduce + (map #(Math/abs (- mean %)) data)) N)]
    (- mean (* (config :threshold-strength) mad))))


