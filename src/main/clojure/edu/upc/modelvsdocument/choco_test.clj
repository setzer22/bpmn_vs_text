(ns edu.upc.modelvsdocument.choco-test
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint]
        [loco.core]
        [loco.constraints])
  (:require [schema.core :as s :refer [fn defn defrecord]]))

(def sim-matrix     [[0.1  0.05 0.0  0.1]
                     [0.5  0.2  0.2  0.0]
                     [0.2  0.45 0.1  0.0]
                     [0.3  0.2  0.6  0.0]
                     [0.2  0.2  0.5  0.8]])

(def sentence-order [[:!= :-> :-> :-> :->]
                     [:<- :!= :-> :-> :-> ]
                     [:<- :<- :!= :-> :->]
                     [:<- :<- :<- :!= :->]
                     [:<- :<- :<- :<- :!=]])

(def task-order     [[:!= :-> :-> :->]
                     [:<- :!= :!= :->]
                     [:<- :!= :!= :->]
                     [:<- :<- :<- :!=]])

(defn matrix-float-to-int :- IntCostMatrix 
  "Matrix is a [0..1] matrix, returns a matrix of integer elements
   where the average value in matrix has a value of resolution"
  [matrix :- CostMatrix, resolution :- s/Int]
  (let [avg (/ (reduce + (flatten matrix)) 
               (* (count matrix) 
                  (count (get matrix 0))))
        coef (* resolution (/ 1 avg))]
    (transform [ALL ALL] #(int (* coef %)) matrix)))

;(find-solution sim-matrix sentence-order task-order)
(defn strategy-1 [sim-matrix :- CostMatrix 
                  sentence-order :- OrderMatrix 
                  task-order :- OrderMatrix] 
  (let [sim-matrix (matrix-float-to-int sim-matrix 1000)

        tst (zip [4 10 3 6 10 7 0 4 3 8 11 8 0 3 6 1 5 3] (range)) 
        __ (reduce + (map #(apply get-matrix sim-matrix %) tst))
        
        T          (count task-order) ; number of tasks
        S          (count sentence-order) ; number of sentences

        max-score  (apply max (flatten sim-matrix))
        
        tasks      (fn [] 
                     (for [t (range T)] [:task t]))
        sentences  (fn []
                     (for [s (range S)] s))
        sims       (fn []
                     (for [t (range T)] [:_sim t]))
        similarity (fn [s [_ t]]
                     (get-matrix sim-matrix s t)) 
        assigned?  (fn [t s]
                     ($= t s))
        get-task-order 
                   (fn [[_ t1] [_ t2]]
                     (get-matrix task-order t1 t2))
        get-sentence-order
                   (fn [s1 s2]
                     (get-matrix sentence-order s1 s2))
        no-crossings 
        (for [t1 (tasks), t2 (tasks)
              :when (= (get-task-order t1 t2) :->)]
          ($<= t1 t2))
        ;(remove nil? 
                ;(for [t1 (tasks), t2 (tasks)
                      ;s1 (sentences), s2 (sentences) 
                      ;:when (= (get-task-order t1 t2) :->)]
                  ;(if (= (get-sentence-order s1 s2) :<-)
                    ;($not ($and (assigned? t2 s2) (assigned? t1 s1))))))

        sim-is-similarity 
        (flatten (for [[t sim] (zip (tasks) (sims))] 
                   (for [s (sentences)]
                     ($if (assigned? t s) 
                          ($= sim (similarity s t))))))
        model 
        (concat 
          (for [t (tasks)] ($in t 0 (- S 1)))
          (for [s (sims)] ($in s (flatten sim-matrix)))
          sim-is-similarity
          no-crossings)

        sol (comment (solution model
              :maximize (apply $+ (sims))
              ))
        
        st-pairs (map (fn [[[_ t] s]] [s t]) sol)]
    st-pairs
    ))


(defn strategy-2 [sim-matrix :- CostMatrix 
                  sentence-order :- OrderMatrix 
                  task-order :- OrderMatrix] 
  (time (let 
          [int-sim-matrix (matrix-float-to-int sim-matrix 1000)
           T          (count task-order) ; number of tasks
           S          (count sentence-order) ; number of sentences

           ;Note: The order in which assigns get generated
           ; is very important, because they will get multiplied
           ; by the flattened matrix.
           assigns (for [s (range S), t (range T)]
                     [:assigned t s])

           get-task-order 
           (fn [t1 t2]
             (get-matrix task-order t1 t2))
           get-sentence-order
           (fn [s1 s2]
             (get-matrix sentence-order s1 s2))

           var-domains (for [a assigns] ($in a 0 1))

           is-mapping (for [t (range T)]
                        (let [t-to-all-s (for [s (range S)] [:assigned t s])] 
                          ($= (apply $+ t-to-all-s) 1)))

           no-crossings (remove-nil
                          (for [s (range S), t (range T)
                                s' (range S), t' (range T) 
                                :when (= :-> (get-task-order t t'))]
                            (if (= :<- (get-sentence-order s s'))
                             ; ($not ($and ($= 1 [:assigned t s]) ($= 1 [:assigned t' s'])))
                             ; NOTE: This version is faster
                              ($< ($+ [:assigned t s] [:assigned t' s']) 2))))

           model (concat 
                   var-domains
                   is-mapping
                   [($= (apply $+ assigns) T)]
                   (comment no-crossings))

           sol (solution model
                         :maximize ($scalar assigns (flatten int-sim-matrix))
                         :timeout 10000)

           st-pairs (map (fn [[[_ t s] _]] [s t]) (filter (fn [[_ v]] (= 1 v)) sol))
           ] 
          ;(println (map (fn [[[_ t s] score]] (str "A_" s "_" t "*" score "\n")) (zip assigns (flatten sim-matrix))))
          ;(println (filter (fn [[_ v]] (= 1 v)) sol))
          st-pairs)))


(defn generate-ilp-model [sim-matrix :- CostMatrix
                          sentence-order :- OrderMatrix
                          task-order :- OrderMatrix]
  (spit "/home/josep/Repositories/bpmn_vs_text/BPMNvsText/ilp-test.lp"
    (let [sim-matrix (matrix-float-to-int sim-matrix 1000)
          T          (count task-order) ; number of tasks
          S          (count sentence-order) ; number of sentences

          variables (for [s (range S), t (range T)] (str "a_" s "_" t))
          assigned (fn [s t] (str "a_"s "_"t))

          get-task-order (fn [t1 t2]
                           (get-matrix task-order t1 t2))
          get-sentence-order (fn [s1 s2]
                               (get-matrix sentence-order s1 s2))
          ]
      (with-out-str
        (println "Maximize")
        (println (clojure.string/join " + " (map #(str %1 " " %2) (flatten sim-matrix) variables))) 
        (println "Subject To")
        (println 
          (clojure.string/join 
            "\n" 
            (for [s (range S), t (range T)
                  s' (range S), t' (range T) 
                  :when (and (= :-> (get-task-order t t'))
                             ;(= :<- (get-sentence-order s s'))
                             (> s s')
                             )]
              (str "n_"s"_"s'"_"t"_"t'": " (assigned s t) " + " (assigned s' t') " <= 1"))))
        (println 
          (clojure.string/join 
            "\n"
            (for [t (range T)] 
              (str (clojure.string/join " + " (for [s (range S)] (assigned s t))) " = 1"))))
        (println "Bounds")
        (println (clojure.string/join 
                   "\n" 
                   (map #(str "0 <= " % " <= 1") variables)))
        (println "Binary")
        (println (clojure.string/join " " variables))
        (comment (println "SOS"))
        (comment (println 
          (clojure.string/join 
            "\n" 
            (for [t (range T)] (str "sos"t": S1 :: " 
                                    (clojure.string/join " " (for [s (range S)] (str (assigned s t) " : " s))))))))
        (println "End")))))
