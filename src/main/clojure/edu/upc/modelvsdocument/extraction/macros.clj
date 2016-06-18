(ns edu.upc.modelvsdocument.extraction.macros
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.config])
  (:require [schema.core :as schema :refer [fn defn defrecord]]
            [clojure.core.match :as match :refer [match]]))

(defmacro model-extractor [name args body] 
  `(defn ~name ~args ~body))

; Mini DSL to define new extractors
(defmacro defextractors [extractor-args & extractor-defs]
  (concat 
    [`do]
    (map (fn [[name body]] `(model-extractor ~name ~extractor-args ~body)) extractor-defs) 
    (list 
      `(let [~'feature-extractors ~(mapv first extractor-defs)] 
         (defn ~'extract-features ~['model :- `BPMN, 'analyzed-model :- `Text]
           (map 
             (fn [~'task-id] (flatten (map #(% ~'task-id ~'model ~'analyzed-model) ~'feature-extractors)))
             (map #(.getId %) (bpmn/all-tasks ~'model))))))))

; Mini DSL to define new features. Semantics are:

;(deffeatures
;  (feature-name [arg1 arg2 ... argN] :weight weights
;     "Explanatory string %2$s %3$s")) ; Args will be applied in order to 
;                                        the explanatory string using a java formatter

; NOTE: Explanatory strings and weights are overriden from the config file

(defmacro deffeatures [& features]
  (let [names (map first features)
        weights (map #(nth % 3) features)
        explanations (map #(nth % 4) features)
        explanation-arguments (map #(nth % 1) features)
        feature-weights (gensym 'feature-weights)
        mk-feature (gensym 'mk-feature)
        fweights-def `(def ~feature-weights ~(into {} (map vector (map keyword names) weights)))
        mkfeature-def `(defn ~mk-feature 
                         ([ftype# argument#]
                          (Feature. ftype# argument# (if (contains? (config :feature-weight-overrides) ftype#) 
                                                          (ftype# (config :feature-weight-overrides))
                                                          (ftype# ~feature-weights))))
                         ([ftype# argument# weight-mul#]
                          (Feature. ftype# argument# (* weight-mul# 
                                                        (if (contains? (config :feature-weight-overrides) ftype#) 
                                                          (ftype# (config :feature-weight-overrides))
                                                          (ftype# ~feature-weights))))))
        ; Hack: The explain-feature multimethod is defined in a namespace that requires
        ;       this macro.
        explain-feature 'edu.upc.modelvsdocument.extraction.feature/explain-feature 
        feature-funcs-def (apply list 'do 
                                (map (fn make-func [n] 
                                       `(defn ~(symbol n) 
                                          ([arg#] (~mk-feature ~(keyword n) arg#))
                                          ([arg# weight#] (~mk-feature ~(keyword n) arg# weight#)))) 
                                     names))
        feature-explain-def (conj (for [[n e a] (map vector names explanations explanation-arguments)]
                                    `(defmethod ~explain-feature ~(keyword n) [{~a :argument}]
                                         (apply format 
                                                (if (contains? (config :feature-explain-overrides) ~(keyword n))
                                                  (~(keyword n) (:feature-explain-overrides config))
                                                  ~e)
                                                ~a))) 'do)]
    (list 'do fweights-def mkfeature-def feature-funcs-def feature-explain-def)))

;; Constituents pattern matching language

; Syntax
; (tree-match [value-to-match]
;   tree-pattern-1 something-1
;   tree-pattern-2 something-2
;   ...
;   tree-pattern-n something-n) 

; Example syntax
;(tree-match [(enrich-with-tokens s2 (first (:constituents s2)))]
      ;["claus" ["adv" "if"] & rst] 
      ;(do-something rst)

      ;["sub-cl" "if" & rst] 
      ;(do-something rst))

(defn se [n]
  (match [n] 
    [[root & children]] {:label root
                         :children (mapv se children)}
    [node] (cond
            (string? node) {:leaf "1", :token {:lemma node}}
            (symbol? node) node)))

(defmacro se-match [m & bindings]
  (let [bindings' (mapcat (fn [[fst snd]] [(if (= :else fst) :else [(se fst)]) snd]) (partition 2 bindings))] 
    `(match ~m
            ~@bindings')))

(defmacro tree-match [[T] & bindings]
  (assert (even? (count bindings)) "You must provide an even number of patterns, each with its result")
  (assert (every? #(not= :else %) (map first (partition 2 bindings))) "No else clause is allowed.")
  (assert (every? not-nil? (map second (partition 2 bindings))) "No pattern can return nil as a value.")
  (let [bindings' (concat bindings [:else nil])] 
    `((fn treematches# [{label# :label children# :children :as T#}] 
        (let [match# (se-match [T#] 
                               ~@bindings')] 
          (cond 
            match# match#
            (and label# children#) (remove nil? (flatten (map #(treematches# %) children#)))
            :else nil))) 
      ~T)))

