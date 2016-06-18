(ns edu.upc.modelvsdocument.config
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint])
  (:require [schema.core :as s :refer [fn defn defrecord]]
            [clojure.java.io :as io]
            [clojure.walk :as walk]))

; Credentials are stored separately because it is sensitive data. 
; TODO: Password should be encrypted
(def ^:dynamic *credentials-path* (str (System/getenv "HOME") "/.textserver-credentials"))
(defn set-creds-path! [creds-path]
  (alter-var-root #'*credentials-path* (constantly creds-path)))

(def config 
  {:hiperonimy-chain-length 4
   :hiperonimy-multiplier 0.5
   :feature-weight-overrides {}
   :feature-explain-overrides {}
   :similarity-function "weighted-fake-jaccard"
   :threshold-strength 1.0
   :verbose-log false
   })

(def config-schema 
  {:hiperonimy-chain-length s/Int
   :hiperonimy-multiplier s/Num
   :feature-weight-overrides {s/Keyword s/Num}
   :feature-explain-overrides {s/Keyword s/Str}
   :similarity-function s/Str
   :threshold-strength s/Num
   :verbose-log s/Bool})

(defn validate-config [config]
  (try (s/validate config-schema config)
       (catch Exception e
         (throw (Exception. "Config file has invalid syntax.")))))

(defn set-value-in-config [str-key value]
  (if-not (contains? config (keyword str-key)) (throw (Exception. (str "No valid config key: " str-key))))
  (let [new-value (if (map? value) (walk/keywordize-keys value) value)
        new-config (assoc config (keyword str-key) value)]
    (validate-config new-config)
    (def config new-config)))

(defn get-value-in-config [str-key]
  (assert (contains? config (keyword str-key)))
  ((keyword str-key) config))

;TODO: Stronger validation of config and nicer error messages. This is just type checking
(defn load-config-from-file [path]
  (binding [*read-eval* false] ; Avoid code execution when reading config file
    (when-not (.exists (io/file path)) (Exception. (str "Config file: " path ", doesn't exist.")))
    (let [config-str (slurp (io/file path))
          new-config (walk/keywordize-keys (read-string config-str))]
      (validate-config new-config)
      (def config new-config))))

(comment 
  (load-config-from-file "/home/josep/.bpmnvstext.config"))

