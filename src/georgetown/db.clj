(ns georgetown.db
  (:require
    [datalevin.core :as d]
    [georgetown.schema :as schema]))

(defonce conn-atom (atom nil))

(defn connect! []
  (reset! conn-atom
          (d/get-conn "data/datalevin"
            (schema/->datalevin schema/schema))))

(defn conn []
  (if (nil? @conn-atom)
    (connect!)
    @conn-atom))

(defn remove-nil-vals [m]
  (->> m
       (filter (fn [[_k v]]
                 (some? v)))
       (into {})))

(defn transact! [& args]
  (apply d/transact (conn) args))

(defn q [query & args]
  (apply d/q query @(conn) args))

#_(connect!)

;; all
#_(d/q '[:find [?e ...]
         :where [?e _ _]]
       @(conn))

#_(d/q '[:find [ (pull ?e [*
                           { :offer/_improvement [ *]}
                           ]) ...]
         :where [?e :improvement/id _]]
       @(conn))

;; drop all
#_(d/clear @conn-atom)

;; TODO close when app closes
;; or else lock gets stuck
#_(d/close @conn-atom)

;; reset
#_(do
    (d/clear @conn-atom)
    (reset! conn-atom nil)
    (connect!))

(defn retract-all! []
  (transact!
    (map (fn [e] [:db/retractEntity e])
         (q '[:find [?e ...]
              :where [?e _ _]])))
    nil)
