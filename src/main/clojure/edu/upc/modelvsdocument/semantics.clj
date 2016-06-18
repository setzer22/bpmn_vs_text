(ns edu.upc.modelvsdocument.semantics
  (:gen-class)
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clj-xpath.core :as xpath]))

; GLOBAL TODO LIST:
;   - POS filter not working properly
;   - Detect references in-sentence
;   - Detect co-references accross sentences


; DATA STRUCTURES

;; A Sentence instance represents a sentence in the text. 
;;    bow - A list of all the non-stopword token ids in the Sentence
;;    predicates - A list of all the predicates in the Sentence
;;    tokens - A map of token details indexed by id, same format as textserver
(defrecord Sentence [bow predicates tokens])

; TEST PARAMETERS

(def url "http://frodo.lsi.upc.edu:8080/TextWS/textservlet/ws/processQuery/semgraph")
(def username "jsanchezf")
(def password "Freeling1234!")
(def text "A small company manufactures customized bicycles. Whenever the sales department receives an order, a new process instance is created. A member of the sales department can then reject or accept the order for a customized bike. In the former case, the process instance is finished. In the latter case, the storehouse and the engineering department are informed. The storehouse immediately processes the part list of the order and checks the required quantity of each part. If the part is available in-house, it is reserved. If it is not available, it is back-ordered. This procedure is repeated for each item on the part list. In the meantime, the engineering department prepares everything for the assembling of the ordered bicycle. If the storehouse has successfully reserved or back-ordered every item of the part list and the preparation activity has finished, the engineering department assembles the bicycle. Afterwards, the sales department ships the bicycle to the customer and finishes the process instance.")
(def lang "en")

; CALLING THE TEXTSERVER
;;    This code calls the TextService and returns its response as an string in xml. Use 
;;    analyze-cached for a cached version so we don't spam the TextService

(defmacro text-body [k v]
  "Shortcut macro to make multipart text-body parameters"
  {:name k, :content v})

(defn analyze-with-textservice [text lang username password] 
  "Returns an xml-formatted string with the TextServer answer"
    (client/post url
      {:multipart 
        [(text-body "username" username) 
         (text-body "password" password)
         (text-body "text_input" text)
         (text-body "language" lang)
         (text-body "output" "xml")
         (text-body "interactive" "1")]}))

(def analyze-cached 
  "Same as analyze-with-textservice, cached version"
  (memoize analyze-with-textservice))

(defn get-token [xmlsentence token-id] 
  "Returns the xml structure for the token with token-id in xmlsentence"
  (xpath/$x:attrs (str "./token[@id=\"" token-id "\"]") xmlsentence))

;TODO: I should refactor map-by to take transformation functions to make it more generic
;@UtilFunction
(defn map-by [lst k]
  "lst is a list of maps, returns a map lst's elements indexed by their k key"
  (reduce 
    (fn [new-map e] (assoc new-map (get e k) e))
    {}
    lst))

(defn get-token-map [xmlsentence] 
  "Returns a map of tokens indexed by id"
    (map-by (map :attrs (xpath/$x ".//token" xmlsentence)) :id))

(defn token-num [token] (Integer. (second (clojure.string/split token #"\."))))
(defn token-sentence [token] (Integer. (.substring (first (clojure.string/split token #"\.")) 1)))
(defn token-interval [from to]
  "Given a from and to token ids in TextService's format, return the range of ids between them"
  (map #(str "t" (token-sentence from) "." %)
       (range (token-num from) (inc (token-num to)))))

(def pos-filter 
  "POS tags in this list are considered stopwords and are removed from the sentence"
  ["punctuation", "preposition", "determiner", "modal"])

;@UtilFunction
(defn in? [seq elm]  
  "True if seq contains elm"
  (some #(= elm %) seq))

(defn clean-sentence [xmlsentence token-ids]
  "Removes stopwords from a token list"
  (filter (fn [token-id] 
            (not (in? pos-filter (:pos (get-token xmlsentence token-id)))))
          token-ids))

(defn extract-bow [xmlsentence]
  "Given a sentence node in xml returns a lemmatized bag of words from its non-stopword tokens"
  (clean-sentence xmlsentence (map #(get-in % [:attrs :id]) (xpath/$x ".//token" xmlsentence))))

(defn extract-predicate [predicate] 
  "Given a predicate node in xml returns a map structure representing it as a map with fields,
  each containing lists of words"
  (reduce
    (fn [nmap argument] (assoc nmap (:role argument) (token-interval (:from argument) (:to argument)))) 
    {:verb (seq [(get-in predicate [:attrs :head_token])])}
    (map :attrs (xpath/$x "./argument" predicate))))

(defn extract-predicates [xmlsentence] 
  "Given a sentence in xml extracts its predicates and return a list"
  (map #(extract-predicate %) (xpath/$x ".//predicate" xmlsentence)))

(defn transform-text [text lang username password] 
  "Calls the textserver and returns a list of Sentence structures, one for each sentence in the text"
  (let 
    [response       (analyze-cached text lang username password)
     response-body  (xpath/xml->doc (:body response))
     sentences      (xpath/$x "//sentence" response-body)
     extract-sentence (fn [xmlsentence] (map->Sentence {:bow (extract-bow xmlsentence), :predicates  (extract-predicates xmlsentence), :tokens (get-token-map xmlsentence)}))]
    ;in
    (map extract-sentence sentences)))

;; TEST DATA
;(def freeling-xml (clojure.string/trim-newline (:body (analyze-cached text lang username password))))
;(transform-text text lang username password)

