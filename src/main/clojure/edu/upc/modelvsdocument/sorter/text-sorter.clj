(ns edu.upc.modelvsdocument.sorter.text-sorter
  (:refer-clojure :exclude [fn defn defrecord])
  (:require [schema.core :as s :refer [defn fn defrecord]])
  (:use [com.rpl.specter] 
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.schemas])
  (:gen-class))

(defn build-order-matrix :- OrderMatrix
  [n :- s/Int]
  (mapv 
    (partial into []) 
    (partition n (for [i (range n), j (range n)]
                   (cond 
                     (> i j) :->
                     (= i j) :!=
                     (< i j) :<-)))))
