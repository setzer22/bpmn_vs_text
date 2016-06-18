(ns edu.upc.modelvsdocument.logic-test
  (:use [clojure.core.logic]
        [clojure.core.logic.pldb]
        [clojure.core.logic.protocols])
  (:require [clojure.core.logic.fd :as fd]))

(def sim-matrix 
  [[0.4 0.5 0.1]
   [0.1 0.2 0.5]])

(defn transitive [r]
  (fn t [p1 p2]
    (fresh [between]
      (conde 
        [(r p1 p2)]
        [(r p1 between)]
        [(t between p2)]))))


(db-rel before-fact x y)
(def facts 
  (db 
    [before-fact :t0 :t1]
    [before-fact :t1 :t2]))

(def tasks [:t0 :t1 :t2])

(defn tasko [x]
  (membero x tasks))

(run* [q] (tasko q))

(defn before [x y]
  (conde
    [(before-fact x y)]
    [(fresh [z]
       (tasko z)
       (before x z)
       (before z y))]))

(defn after [x y]
  (before y x))

(with-db facts 
  (run 3 [q]
    (after q :t0)))


(defn sumo [lst s]
  (conde 
    [(fresh [h t s'] 
       (conso h t lst)
       (fd/+ h s' s)
       (sumo t s'))]
    [(== [] lst)
     (== 0 s)]))

(run* [s] (sumo [1 2 3 4 5] s))

(defn subseto [lst sub]
  (conde 
    [(== lst []) (== sub [])]
    [(fresh [h1 t1 h2 t2]
        (conso h1 t1 lst)
        (conso h2 t2 sub)
        (== h1 h2)
        (subseto t1 t2))]
    [(fresh [h1 t1 h2 t2]
        (conso h1 t1 lst)
        (conso h2 t2 sub)
        (!= h1 h2)
        (subseto t1 sub))]))

