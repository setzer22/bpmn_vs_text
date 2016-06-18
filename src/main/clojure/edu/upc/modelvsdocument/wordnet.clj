(ns edu.upc.modelvsdocument.wordnet
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint])
  (:require [schema.core :as s :refer [fn defn defrecord]]
            [clojure.set :as set :refer [map-invert]]))

(defn read-dictionaries [wordnet-path] 
  (let [relations-lines (map #(clojure.string/split % #" ") 
                             (line-seq (clojure.java.io/reader (str wordnet-path "wn30.src"))))
        senses-lines    (map #(clojure.string/split % #" ")
                             (line-seq (clojure.java.io/reader (str wordnet-path "senses30.src"))))
        wn->fullname    (into {} (map (fn [[wn-id & senses]] [wn-id, senses]) senses-lines))
        wn->name        (into {} (map (fn [[fst [fst-sense & _]]] [fst fst-sense]) wn->fullname))
        wn->parent      (into {} (map (fn [[child parent & _]] 
                                                        [child, (when (not= "-" parent) parent)]) 
                                                      relations-lines))
        wn->children    (dissoc 
                          (reduce (fn [rev-map [k v]]
                                    (merge-with 
                                      concat
                                      rev-map
                                      {v [k]})) 
                                  {}
                                  wn->parent) 
                          nil)] 
    {:all-senses wn->fullname
     :sense wn->name
     :parent-of wn->parent
     :children-of wn->children}))

(def wordnet nil)

;TODO: Just for REPL work
(if (and (.exists (clojure.java.io/as-file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/wordnet/wn30.src"))
         (.exists (clojure.java.io/as-file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/wordnet/senses30.src")))
  (def wordnet (read-dictionaries "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/wordnet/")))

(defn read-wordnet-dictionaries [wordnet-path]
  (def wordnet (read-dictionaries wordnet-path)))

(defn senses-of [wn-id]
  (if wordnet 
    (get (get wordnet :all-senses) wn-id)))

(defn sense-of [wn-id]
  (if wordnet 
    (get (get wordnet :sense) wn-id)))

(defn parent-of [wn-id]
  (if wordnet 
    (get (get wordnet :parent-of) wn-id)))

(defn children-of [wn-id]
  (if wordnet 
    (get (get wordnet :children-of) wn-id)))

(defn hiperonimy-chain-of [wn-id]
  (let [parent (parent-of wn-id)] 
    (lazy-seq (cons parent (if parent (hiperonimy-chain-of parent) [])))))

