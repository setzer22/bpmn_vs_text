(ns edu.upc.modelvsdocument.sorter.model-sorter2
  (:refer-clojure :exclude [fn defn defrecord])
  (:require [edu.upc.modelvsdocument.bpmn :as bpmn]
            [schema.core :as s :refer [defn fn defrecord]]
            [clojure.set :as set])
  (:use [com.rpl.specter] 
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.bpmn])
  (:gen-class))

