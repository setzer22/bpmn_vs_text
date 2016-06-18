(ns edu.upc.modelvsdocument.textserver
  (:refer-clojure :exclude [fn defn defrecord])
  (:use [com.rpl.specter]
        [edu.upc.modelvsdocument.schemas]
        [edu.upc.modelvsdocument.utils])
  (:require [edu.upc.modelvsdocument.config :as config] 
            [schema.core :as s :refer [fn defn defrecord]]
            [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.walk :as walk])
  (:gen-class))

; CALLING THE TEXTSERVER
;;    This module calls the TextServer and returns its response as an string in json. Use 
;;    analyze-cached for a cached version so we don't spam the TextServer

(def url 
  "The textserver url"
  "http://frodo.lsi.upc.edu:8080/TextWS/textservlet/ws/processQuery/semgraph")
(def lang 
  "The language used for analysis"
  "en")

(defn text-body [k v]
  "Returns a multipart text-body parameter"
  {:name k, :content v})

(defn analyze-with-textserver [text lang] 
  "Returns an xml-formatted string with the TextServer answer"
  (let [{:keys [username password]} (read-string (slurp config/*credentials-path*))]
    (assert (not-any? nil? [username password]))
    (:body 
      (client/post url
                   {:multipart 
                    [(text-body "username" username) 
                     (text-body "password" password)
                     (text-body "text_input" text)
                     (text-body "language" lang)
                     (text-body "output" "json")
                     (text-body "interactive" "1")]}))))

(def analyze-cached 
  "Same as analyze-with-textserver, cached version"
  (memoize analyze-with-textserver))

(defn textserver->json [result]
  (walk/keywordize-keys (json/read-str result)))
