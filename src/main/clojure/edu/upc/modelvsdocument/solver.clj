(ns edu.upc.modelvsdocument.solver
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [edu.upc.modelvsdocument.utils]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils]
        [clojure.pprint]
        [loco.core]
        [loco.constraints])
  (:require [schema.core :as s :refer [fn defn defrecord]])
  (:import [lpsolve LpSolve]))

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

(defn solve-with-lpsolve [sim-matrix :- CostMatrix
                          sentence-order :- OrderMatrix
                          task-order :- OrderMatrix]
  (try 
    (let [sim-matrix (matrix-float-to-int sim-matrix 1000)
        T          (count task-order) ; number of tasks
        S          (count sentence-order) ; number of sentences
        ST         (* S T)

        all-variables (for [s (range S), t (range T)] [s t])
        assigned (fn [s t] (str "a_"s "_"t))

        get-task-order (fn [t1 t2]
                         (get-matrix task-order t1 t2))
        get-sentence-order (fn [s1 s2]
                             (get-matrix sentence-order s1 s2))

        ; Initial solver with T variables.
        solver (LpSolve/makeLp 0 (* S T))
        

        ; An improvised local-API to make constraints:
        ; The Left-Hand-Side (LHS) of a constraints is a sum of terms, represented as an array 
        ; The Right-Hand-Side (RHS) is a constant value
        ; A constraint is always of the form LHS cmp RHS with comp in {<=, >=, =, !=}
        assigned (fn [s t] [s t])
        make-lhs (fn [] (vec (repeat ST 0)))
        add-term (fn [lhs s t coef] (assoc lhs (+ (* s T) t) coef))
        +vars-coefs (fn [variables coefficients]
                   (let [row (make-lhs)]
                     (reduce (fn [row [[s t] coef]] (add-term row s t coef))
                             row
                             (zip variables coefficients))))
        +vars (fn [& variables] (+vars-coefs variables (repeat 1)))
        lhs->str (fn [constr] (clojure.string/join " " constr))
        cmp->lpsolve {:>= LpSolve/GE, :<= LpSolve/LE, := LpSolve/EQ}
        add-constraint (fn [lhs cmp value]
                         (.strAddConstraint solver (lhs->str lhs) (cmp->lpsolve cmp) value))
        ]

    ; All variables are binary
    (doseq [t (range ST)]
      ; We set variables from 1 to ST because column 0 is the objective function
      (.setBinary solver (+ t 1) true))

    ; Order constraints
    (doseq [s (range S), t (range T)
            s' (range S), t' (range T) 
            :when (and (= :-> (get-task-order t t'))
                       (> s s'))]
      (add-constraint 
        (+vars (assigned s t) (assigned s' t')) :<= 1))

    ; Only one sentence per task
    (doseq [t (range T)] 
      (add-constraint 
        (apply +vars (for [s (range S)] (assigned s t))) := 1))

    ; Objective function
    (.setMaxim solver)
    (.strSetObjFn solver (lhs->str (+vars-coefs all-variables (flatten sim-matrix))))

    ;TODO: Add SOS constraints?
    (.solve solver)

    (let [result (into [] (.getPtrVariables solver))
          st-pairs (for [s (range S), t (range T)
                         :when (= 1.0 (result (+ (* s T) t)))]
                     [s t])]
      (.deleteLp solver)
      st-pairs))
    (catch java.lang.UnsatisfiedLinkError e
      (throw (Exception. "The libraries needed to run lp_solve were not found in any of the java.library.path folders.")))))

(defn generate-cplex-lp-model [sim-matrix :- CostMatrix
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

;STRATEGY 1: Not used
(comment
  (defn strategy-1 [sim-matrix :- CostMatrix 
                    sentence-order :- OrderMatrix 
                    task-order :- OrderMatrix] 
    (let [sim-matrix (matrix-float-to-int sim-matrix 1000)
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

          sol (solution model
                        :maximize (apply $+ (sims)))

          st-pairs (map (fn [[[_ t] s]] [s t]) sol)]
      st-pairs
      )))


;STRATEGY 2: Better modeled as ILP
(comment (defn strategy-2 [sim-matrix :- CostMatrix 
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
          st-pairs))))


