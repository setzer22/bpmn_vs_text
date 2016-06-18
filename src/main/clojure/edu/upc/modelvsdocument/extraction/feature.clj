(ns edu.upc.modelvsdocument.extraction.feature
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.extraction.macros])
  (:require [schema.core :as s :refer [fn defn defrecord]]
            [edu.upc.modelvsdocument.wordnet :as wn])
  (:gen-class))

(defrecord Feature [ftype :- s/Keyword, argument, weight :- s/Num]
  java.lang.Comparable
  (compareTo [this f] 
    (let [cmp (compare (:ftype this) (:ftype f))] 
      (if 
        (zero? cmp)
        (compare (:argument this) (:argument f))
        cmp))))

; Feature comparison should not use = but this:
(defn feature= [f1 :- Feature, f2 :- Feature]
  (= 0 (compare f1 f2)))

; Override pprint behaviour 
(defn pprint-feature [{argument :argument ftype :ftype weight :weight} :- Feature]
  (print (str weight "*" (name ftype) (if (vector? argument) (seq argument) (str "(" argument ")")))))
(. clojure.pprint/simple-dispatch addMethod Feature pprint-feature)

; Multimethod for feature explanatory string
(defmulti explain-feature :ftype)
(defmethod explain-feature :default [f] (pprint-feature f))

; Feature definition (see deffeatures macro)
(deffeatures
    (has-lemma [pos lemma] :weight 1.5
      "Contains the %1$s \"%2$s\"")

    (has-action [action] :weight 5.0
      "Contains the action \"%s\"")

    (agent-head [lemma pos verb] :weight 20.0
      "The agent of verb \"%3$s\" is \"%1$s\" (%2$s)")

    (patient-head [lemma pos verb] :weight 20.0
      "The direct object of verb \"%3$s\" is \"%1$s\" (%2$s)")

    (in-agent [lemma pos verb] :weight 4.0
     "The agent of verb \"%3$s\" contains the %2$s \"%1$s\"")
    
    (in-patient [lemma pos verb] :weight 4.0
      "The object of verb \"%3$s\" contains the %2$s \"%1$s\"")

    (has-synset [synset] :weight 1.0
      "Contains the synset %s")
    
    (has-parent-synset [synset] :weight 0.3
      "Contains the synset %s")
    
    (lemma-conditional-pred [lemma pos] :weight 4.0
      "Precedes a conditional clause containing the %2$s \"%1$s\"  ")

    (lemma-conditional-follow [lemma pos] :weight 4.0
      "Goes after a conditional clause containing the %2$s \"%1$s\"  "))

; NOTE: Support for complex features that need more info than its arguments 
;       is not supported by the deffeatures DSL. So special cases should
;       be added manually.
(defmethod explain-feature :has-synset [{[wn-id] :argument}]
  (str "Contains the synset \"" (let [s (wn/sense-of wn-id)]
                                  (if s s wn-id)) "\""))

(defmethod explain-feature :has-parent-synset [{[wn-id] :argument}]
  (str "Contains an hyponym of \"" (let [s (wn/sense-of wn-id)]
                                  (if s s wn-id)) "\""))
