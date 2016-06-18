(ns edu.upc.modelvsdocument.core
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [clojure.pprint]
        [edu.upc.modelvsdocument.utils]
        [edu.upc.modelvsdocument.config])
  (:require [schema.core :as s :refer [fn defn defrecord]]
            [edu.upc.modelvsdocument.sorter.model-sorter :as model-sorter]
            [edu.upc.modelvsdocument.sorter.text-sorter :as text-sorter]
            [clojure.string :as string]
            [edu.upc.modelvsdocument.utils :as utils]
            [edu.upc.modelvsdocument.textserver :as textserver]
            [edu.upc.modelvsdocument.bpmn :as bpmn]
            [edu.upc.modelvsdocument.bpmn-analysis :as bpmn-analysis]
            [edu.upc.modelvsdocument.extraction.text-extraction :as textextraction]
            [edu.upc.modelvsdocument.extraction.model-extraction :as modelextraction]
            [edu.upc.modelvsdocument.extraction.feature :as f]
            [edu.upc.modelvsdocument.similarity :as similarity]
            [edu.upc.modelvsdocument.lap :as lap]
            [edu.upc.modelvsdocument.solver :as solver]
            )
  (:import [edu.upc.modelvsdocument.extraction.feature Feature]
           [edu.upc.modelvsdocument ModelManager SentenceIdsPair Result Match]
           [org.activiti.bpmn.model Task StartEvent EndEvent UserTask SequenceFlow BpmnModel]
           )
  (:gen-class))

(require 'spyscope.core)
(s/set-fn-validation! true)

(defn print-title [title] 
  (println (apply str (take (count title) (repeat "="))))
  (println title) 
  (println (apply str (take (count title) (repeat "=")))))

(defmacro section [title & body]
  (concat `(do) `((print-title ~title)) body `((println ""))))

(defn fv-intersection [fv1 :- [Feature], fv2 :- [Feature]]
  (let [sh (shortest fv1 fv2)
        ln (longest fv1 fv2)]
    (mapcat (fn [feat] (filter #(f/feature= feat %) sh)) ln)))

(defn sentence-ids-pair [sentence-index :- s/Int, ids :- [s/Str]]
  (SentenceIdsPair. sentence-index ids))

;RUN MAIN WITH THIS
(comment
  (print *e)
  (def b-path "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/")
  (binding [*print-level* 12] (println (.getLog (main (str b-path "Credit-scoring.bpmn") (slurp (str b-path "Credit-scoring.txt"))))))
  (binding [*print-level* 12] (println (.getLog (main (str b-path "Recourse.bpmn") (slurp (str b-path "Credit-scoring.txt"))))))
  (println (.getLog (main (str b-path "Dispatch-of-goods.bpmn") (slurp (str b-path "Dispatch-of-goods.txt")))))
  (println (.getLog (main (str b-path "Recourse.bpmn") (slurp (str b-path "Recourse.txt")))))
  (println (.getLog (main "/home/josep/regress_-_english_b9048750ab114f7489ffaa2fbd5ae7b0.bpmn" (slurp (str b-path "Recourse.txt")))))
  (println (.getLog (main (str b-path "Self-service-restaurant.bpmn") (slurp (str b-path "Self-service-restaurant.txt")))))
  (println (.getLog (main (str b-path "Bicycle_Manufacturer.bpmn") (slurp (str b-path "Bicycle_Manufacturer.txt")))))
  (println (.getLog (main (str b-path "Computer_Repair.bpmn") (slurp (str b-path "Computer_Repair.txt")))))
  (println (.getLog (main (str b-path "Hotel.bpmn") (slurp (str b-path "Hotel.txt")))))
  (println (.getLog (main (str b-path "Underwriter.bpmn") (slurp (str b-path "Underwriter.txt")))))
  (println (.getLog (main (str b-path "Hospital.bpmn") (slurp (str b-path "Hospital.txt"))))))


(defn main [model-path text] 
  "Runs the algorithm"
  (let [; Read the model, the text is already passed as a string
        model          (bpmn/read-model model-path)

        ; First we analyze the text with the textserver
        text-res       (textserver/textserver->json (textserver/analyze-cached text "en"))
        __ (def text-res text-res) ;TODO: Test code

        ; And we build both the model structure (graph, and so on) and analyze its text
        model-struct   (bpmn/build-model model)
        __ (def model-struct model-struct) ;TODO: Test code
        model-an       (bpmn-analysis/analyze-model-text model-struct text)
        __ (def model-an model-an) ;TODO: Test code

        ; We use the extractor modules to extract the sentences from the text
        text-features  (textextraction/extract-features text-res)
        model-features (modelextraction/extract-features model-struct model-an)

        ; We'll encode the problem as an SxT matrix 
        S (count text-features)
        T (count model-features)

        ; We compute the similarity matrix and its threshold value
        ; TODO: Maybe factor-out the similarity function
        sim-matrix     (seq-to-matrix (for [i text-features, j model-features] 
                                        (similarity/default-similarity i j)) T)
        threshold      (similarity/similarity-matrix-threshold sim-matrix)

        ; We get the order matrices
        sentence-order (text-sorter/build-order-matrix S)
        task-order #spy/d (model-sorter/build-order-matrix model-struct)

        ; We use the solver to find the optimal matching
        choco-matches    (sort-by second (solver/solve-with-lpsolve sim-matrix sentence-order task-order))
        [good-matches, bad-matches] (span (fn [[s t]] (> (get-matrix sim-matrix s t) threshold)) choco-matches)

        disp-sentences (map #(string/join " " (select [:tokens ALL :form] %)) (select [:paragraphs ALL :sentences ALL] text-res))
        disp-tasks     (map #(.getName %) (bpmn/all-tasks model-struct))

        ; Finally, we get the results
        total-score (float (/ (reduce + (map (fn [[s t]] (get-matrix sim-matrix s t)) choco-matches)) T))

        log (with-out-str 
              (section "Sentences" 
                       (doseq [[i s] (zip (range) disp-sentences)] 
                         (println (str "Text." i ") " s))))
              (section "Tasks" 
                       (doseq [[i t] (zip (range) disp-tasks)] 
                         (println (str "Task." i ") " t))))
              (section "Similarity Matrix"
                       (clojure.pprint/print-table (map (fn [row] (into (sorted-map) (map #(vector %1 (format "%.2f" (float %2))) (range) row))) sim-matrix)))
              (section "threshold"
                       (println threshold))
              (section "Matchings"
                       (doseq [[t m] good-matches] 
                         (let [common-features (fv-intersection (nth text-features t) (nth model-features m))]

                           (print "Sentence " t ":\n")
                           (pprint (nth disp-sentences t))
                           (print "Task " m ":\n")
                           (pprint (nth disp-tasks m))
                           (println (str "Matching score: " (nth (nth sim-matrix t) m )))
                           (if (config :verbose-log) 
                             (do (println "\n    Text features: \n")
                                 (doseq [f (nth text-features t)] (println "    * " (f/explain-feature f)))
                                 (println "\n    Model features: \n")
                                 (doseq [f (nth model-features m)] (println "    * " (f/explain-feature f)))))
                           (println "\n    Comon features: \n")
                           (doseq [f common-features] (println (str "    * " (f/explain-feature f))))
                           (println "")
                           (println ""))))
              (section "Tasks with no good match"
                       (doseq [[s t] bad-matches]
                         (print "Task "t ":\n")
                         (pprint (nth disp-tasks t)))))
        matches (mapv
                  (fn [[s t]] 
                    (Match. 
                      s 
                      (nth (bpmn/all-task-ids model-struct) t)
                      (.getName (nth (bpmn/all-tasks model-struct) t))
                      (mapv f/explain-feature 
                                   (fv-intersection (nth text-features s) (nth model-features t)))
                      (get-matrix sim-matrix s t)
                      (in? good-matches [s t])))
                  choco-matches)

        tasks-of-sentence (mapv #(apply sentence-ids-pair %)
                                (zip (range S)
                                     (mapv (fn [s]
                                             (mapv #(.getId (nth (bpmn/all-tasks model-struct) %))  
                                                   (for [[s' t] choco-matches :when (= s s')] t)))
                                           (range S))))]
    ;(def model-struct model-struct)
    ;(def model-an model-an)
    (Result. total-score log matches tasks-of-sentence disp-sentences)))

(defn main-with-files [model-file, text-file]
  (main (.getAbsolutePath model-file) (slurp text-file)))
