(ns georgetown.db
  (:require
    [datalevin.core :as d]
    [georgetown.schema :as schema]))

(def conn
  (delay
    (d/get-conn "data/datalevin"
      (schema/->datalevin schema/schema))))

(defn remove-nil-vals [m]
  (->> m
       (filter (fn [[_k v]]
                 (some? v)))
       (into {})))

(defn transact! [& args]
  (apply d/transact @conn args))

(defn q [query & args]
  (apply d/q query @@conn args))


;; all
#_(d/q '[:find [?e ...]
         :where [?e _ _]]
       @@conn)

;; drop all
#_(d/clear @conn)

(defn retract-all! []
  (transact!
    (map (fn [e] [:db/retractEntity e])
         (q '[:find [?e ...]
              :where [?e _ _]])))
    nil)
