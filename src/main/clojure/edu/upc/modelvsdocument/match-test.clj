(ns edu.upc.modelvsdocument.match-test
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint]
        )
  (:require [schema.core :as s :refer [fn defn defrecord]]
            [clojure.core.match :as match :refer [match]]))


(macroexpand-1 
  '(se-match))



