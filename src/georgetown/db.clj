(ns georgetown.db
  (:require
    [datalevin.core :as d]
    [georgetown.schema :as schema]))

(def conn
  (d/get-conn "data/datalevin"
    (schema/->datalevin schema/schema)))

(defn remove-nil-vals [m]
  (->> m
       (filter (fn [[_k v]]
                 (some? v)))
       (into {})))

