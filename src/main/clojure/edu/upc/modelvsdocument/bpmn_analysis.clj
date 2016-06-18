(ns edu.upc.modelvsdocument.bpmn-analysis
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [clojure.pprint]
        [edu.upc.modelvsdocument.utils])
  (:require [clojure.core.match :as match :refer [match]]
            [schema.core :as s :refer [fn defn defrecord]]
            [clojure.string :as string]
            [edu.upc.modelvsdocument.utils :as utils]
            [edu.upc.modelvsdocument.textserver :as textserver]
            [edu.upc.modelvsdocument.bpmn :as bpmn])
  (:gen-class))


(defn capitalize  [sentence :- s/Str]  
  (if sentence
    (let  [s (seq sentence)]  
      (apply str (conj (rest s) (string/upper-case (first s)))))))

(defn analyze-model-text [model-struct :- BPMN, text :- s/Str] 
  (let [tasks          (bpmn/all-tasks model-struct)
        named-gateways (remove #(empty? (.getName %)) (bpmn/all-gateways model-struct))
        lanes          (bpmn/all-lanes model-struct)
        pools          (bpmn/all-pools model-struct)

        ; All tasks, names and pools should have a valid name. We fail otherwise.
        ; NOTE: We could tolerate this the same way we do with gateways, but that
        ;       would mean restructuring the extraction code since we can't assume
        ;       this anymore
        ;__ (when-not (every? #(not (empty? (.getName %))) pools) 
             ;(throw (Exception. "ERROR: BPMN Model is malformed, all pools should have a name.")))
        ;__ (when-not (every? #(not (empty? (.getName %))) lanes) 
             ;(throw (Exception. "ERROR: BPMN Model is malformed, all lanes should have a name.")))
        __ (when-not (every? #(not (empty? (.getName %))) tasks) 
             (throw (Exception. "ERROR: BPMN Model is malformed, all tasks should have a name.")))

        ;TODO: Temporary patch so experiment 1 works. We will replace ALL missing pool names by some random string
        pools (map #(if (empty? (.getName %)) 
                      (do (.setName % (str "pool" (rand-int 1000000000))) %)
                      %) pools)
        lanes (map #(if (empty? (.getName %)) 
                      (do (.setName % (str "lane" (rand-int 1000000000))) %)
                      %) lanes)

        [T G L P]      [(count tasks) (count named-gateways) (count lanes) (count pools)]

        model-txt      (string/join ".\n" (concat 
                                            (map #(capitalize (.getName %)) tasks)
                                            (map #(capitalize (.getName %)) named-gateways)
                                            (map #(capitalize (.getName %)) lanes)
                                            (map #(capitalize (.getName %)) pools)))
        full-text      (str text ".\n" model-txt)
        model-res      (textserver/textserver->json (textserver/analyze-cached full-text "en"))
        dummy-count     (+ T G L P)

        ; Remove the text sentences from the analyzed model text
        dummy-ids      (mapv :id (take-last dummy-count (select [:paragraphs ALL :sentences ALL] model-res)))

        partition-count (fn partition-count [lst & counts] 
                          (second (reduce 
                            (fn [[count-so-far sublists-so-far] next-count]
                              [(+ count-so-far next-count)
                               (conj sublists-so-far (subvec lst count-so-far (+ count-so-far next-count)))]) 
                            [0, []] 
                            counts)))

        ; Get the ids from the analyzed text
        ; We create a new paragraph with only the sentences from the model
        all-sentences  (select [:paragraphs ALL :sentences ALL] model-res) 

        [_, task-sentences, gateway-sentences, lane-sentences, pool-sentences] 
        (partition-count all-sentences (- (count all-sentences) (+ T G L P)) T G L P)

        good-sentences (concat 
                         (map #(assoc %1 :task-id (.getId %2)) task-sentences tasks)
                         (map #(assoc %1 :gateway-id (.getId %2)) gateway-sentences named-gateways)
                         (map #(assoc %1 :lane-id (.getId %2)) lane-sentences lanes)
                         (map #(assoc %1 :pool-id (.getId %2)) pool-sentences pools))

        new-paragraph  {:sentences good-sentences}
        model-an       (assoc model-res :paragraphs [new-paragraph])

        ;TODO: Delete all this
        ;[task-id? gateway-id? lane-id? pool-id?] (map set (partition-count dummy-ids T G L P))
        ;task-ids       (subvec dummy-ids 0 T)
        ;gateway-ids    (subvec dummy-ids T (+ T G))
        ;lane-ids       (subvec dummy-ids (+ T G) (+ T G L))
        ;pool-ids       (subvec dummy-ids (+ T G L))

        ; We append the task-id to each sentence in the analyzed model sentences
        ; This could be done in a more compact way if var collection worked on multi-paths.
        ;       see: https://github.com/nathanmarz/specter/issues/64

        ;model-an       (transform 
                         ;[:paragraphs ALL :sentences #(task-id? (:id %))]
                         ;(fn [sentences] 
                           ;#spy/d sentences
                           ;(map #(assoc %1 :task-id (.getId %2)) sentences tasks))
                         ;model-res)

        ;model-an       (transform 
                         ;[:paragraphs ALL :sentences ALL #(gateway-id? (:id %))]
                         ;(fn [sentences] 
                           ;(map #(assoc %1 :gateway-id (.getId %2)) sentences named-gateways))
                         ;model-an)

        ;model-an       (transform 
                         ;[:paragraphs ALL :sentences ALL #(lane-id? (:id %))]
                         ;(fn [sentences] 
                           ;(map #(assoc %1 :lane-id (.getId %2)) sentences lanes))
                         ;model-an)

        ;model-an       (transform 
                         ;[:paragraphs ALL :sentences ALL #(pool-id? (:id %))]
                         ;(fn [sentences] 
                           ;(map #(assoc %1 :pool-id (.getId %2)) sentences pools))
                         ;model-an)
                         ]
        
    model-an))
