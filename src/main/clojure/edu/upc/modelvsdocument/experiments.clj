(ns edu.upc.modelvsdocument.experiments
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint]
        [clojure.java.io])
  (:require [schema.core :as s :refer [fn defn defrecord]]
            [edu.upc.modelvsdocument.core :as core]))

(comment 
  ; None of this will be included in the final build 

  (defn extension [file]
    "Returns the file extension of file"
    (let [s (.getName file)]
      (nth (re-find #".*\.(.*)" s) 1)))

  (defn strip-extension [file]
    "Returns the file name without extension"
    (let [s (.getName file)]
      (nth (re-find #"(.*)\.(.*)" s) 1)))

  (defn bpmn-file? [f] (= (extension f) "bpmn"))
  (defn text-file? [f] (= (extension f) "txt"))
  (defn purged-bpmn-file? [f] (= (extension f) "purged"))

  (comment (let [folder (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/")
                 bpmn-files (sort (filter bpmn-file? (file-seq folder)))
                 text-files (sort (filter text-file? (file-seq folder)))]
             (doseq [[m t] (zip bpmn-files text-files)]
               (spit (str "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/logs/" (strip-extension m) ".log") 
                     (.getLog (core/main (.getAbsolutePath m) (slurp t)))))))

  (defmacro measure-time-of 
    "Small utility macro to measure execution time of code"
    [& body]
    `(let [start# (System/nanoTime)
           result# (do ~@body)]
       {:elapsed-time (- (System/nanoTime) start#)
        :result result#}))

  ; =====================
  ;   FIRST EXPERIMENT 
  ; =====================

  (let [benchmark-path "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/seminar-exercices/"
        benchmark-dir (clojure.java.io/file benchmark-path)
        exercises (.listFiles benchmark-dir)]
    (def experiment1-result 
      (doall (for [exercise-dir exercises
                   :let [models (filter purged-bpmn-file? (.listFiles exercise-dir))
                         text (first (filter text-file? (.listFiles exercise-dir)))]]
               {:exercise-name (.getName exercise-dir) 
                :text-file text
                :text-name (.getName text) 
                :results (doall (for [model models]
                                  (try 
                                    (let [result (core/main (.getAbsolutePath #spy/d model) (slurp text))] 
                                      {:model-file model
                                       :model-name (.getName model)
                                       :score (.getTotalScore result)})
                                    (catch Exception e {:model-name (.getName model)
                                                        :score -1}))))}))))

  ;(spit "/home/josep/exp2.clj" (with-out-str (clojure.pprint/pprint experiment1-result)))
  ;(def experiment1-result (read-string (slurp "/home/josep/exp2.clj")))

  (doseq [{:keys [exercise-name results]} experiment1-result
          :let [sorted-results (sort-by :score results)
                top-3 (take-last 3 (remove #(= (:score %) -1) sorted-results))
                worse-3 (take 3 (remove #(= (:score %) -1) sorted-results))]]
    (println (str "Exercise: " exercise-name))
    (println "")
    (println (str "Top 3 in class: "))
    ; NOTE: Sort is done decreasingly
    (clojure.pprint/pprint top-3)
    (println "")
    (println (str "Worst 3 in class: "))
    (clojure.pprint/pprint worse-3))

  ; =====================
  ;   SECOND EXPERIMENT 
  ; =====================


  ; Compute the similarity score for each pair model-text and store it, alongside the execution time, in score-matrix
  (let [benchmark-path "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/"
        benchmark-dir (clojure.java.io/file benchmark-path)
        bpmn-files (sort (filter #(= "bpmn" (extension %)) (file-seq benchmark-dir)))
        text-files (sort (filter #(= "txt" (extension %)) (file-seq benchmark-dir)))
        __  (assert (= (count text-files) (count bpmn-files)))
        T (count text-files)
        M (count bpmn-files)
        score-matrix (seq-to-matrix 
                       (for [t text-files, m bpmn-files
                             :let [text (slurp t)
                                   model-path (.getAbsolutePath m)]]
                         (measure-time-of
                           (.getTotalScore (core/main model-path text)))) 
                       T)]
    (println "Order of files is:")
    (println (map #(.getName %) text-files))
    (println (map #(.getName %) bpmn-files))
    (println "\n")
    (def score-matrix score-matrix)
    (println "The score matrix (rows are texts and columns models)")
    (binding [*print-right-margin* 200] 
      (pprint (transform (walker map?) :result score-matrix))))

  (identity score-matrix)
(binding [*print-right-margin* 200] (pprint (transform (walker map?) :result score-matrix)))
(binding [*print-right-margin* 200] (pprint (transform (walker map?) :elapsed-time score-matrix)))

; =====================
;    3rd EXPERIMENT 
; =====================
(let [model (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Hospital.bpmn")
      text (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Hospital.txt")]
  (spit "/home/josep/Hospital.log" (.getLog (core/main-with-files model text))))

; =====================
;    4th EXPERIMENT 
; =====================

(let [model (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo.bpmn")
      text (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo.txt")]
  (spit "/home/josep/Zoo.log" (.getLog (core/main-with-files model text))))
(let [model (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo.bpmn")
      text (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo1.txt")]
  (spit "/home/josep/Zoo.log" (.getLog (core/main-with-files model text))))
(let [model (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo2.bpmn")
      text (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo.txt")]
  (spit "/home/josep/Zoo2.log" (.getLog (core/main-with-files model text))))
(let [model (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo2.bpmn")
      text (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo2.txt")]
  (spit "/home/josep/Zoo3.log" (.getLog (core/main-with-files model text))))
(let [model (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo3.bpmn")
      text (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo2.txt")]
  (spit "/home/josep/Zoo4.log" (.getLog (core/main-with-files model text))))
(let [model (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo4.bpmn")
      text (file "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/benchmarks/mini-benchmark/Zoo2.txt")]
  (spit "/home/josep/Zoo5.log" (.getLog (core/main-with-files model text))))
)

