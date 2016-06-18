(ns edu.upc.modelvsdocument.test-similarity
  (:use [clojure.test]
        [com.rpl.specter]
        [edu.upc.modelvsdocument.similarity]
        [edu.upc.modelvsdocument.textserver]
        [edu.upc.modelvsdocument.extraction.core]
        [edu.upc.modelvsdocument.extraction.feature]
        [edu.upc.modelvsdocument.extraction.text-extraction]
        [edu.upc.modelvsdocument.utils])
  (:gen-class))

(testing "Cosine Similarity"
  (testing "Equal sentences must have a similarity of 1"
    (let [[s1 s2] (text-to-vector 
                    "The programmer will be sending the file. 
                    The programmer will be sending the file")]
      (is 
        (= 1 (cosine-similarity s1 s2))))))

(testing "Lema extraction"
  (is (= 
        '("small" "company" "manufacture" "customize" "bicycle") 
        (map :argument 
             (extract-lemmas 
               (first (all-sentences
                        (textserver->json (analyze-cached 
                                            "A small company manufactures customized bicycles" "en")))))))))

