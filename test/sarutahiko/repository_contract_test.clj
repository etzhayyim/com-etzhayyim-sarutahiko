(ns sarutahiko.repository-contract-test (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.string :as str] [clojure.test :refer [deftest is]]))
(defn fs [] (filter #(.isFile %) (file-seq (io/file "."))))
(deftest contracts (doseq [f (fs)] (let [n (.getName f) p (.getPath f)] (when (str/ends-with? n ".edn") (is (some? (edn/read-string (slurp f))) p)) (when (re-find #"\.(json|jsonld|bpmn)$" p) (is (or (str/includes? p "/wire/") (str/ends-with? p "/.well-known/did.json")) p)) (is (not (re-find #"\.(go|sh)$" n)) p))))
