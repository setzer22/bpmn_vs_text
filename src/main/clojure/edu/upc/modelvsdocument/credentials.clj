(ns edu.upc.modelvsdocument.credentials
  (:use [edu.upc.modelvsdocument.utils])
  (:gen-class))

(def ^:dynamic *credentials-path* (str (System/getenv "HOME") "/.textserver-credentials"))
(defn set-creds-path! [creds-path]
  (alter-var-root #'*credentials-path* (constantly creds-path)))

