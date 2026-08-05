(ns georgetown.db
  (:require
    [dat.api :as dat]
    [georgetown.schema :as schema]
    [georgetown.config :as config]))

(defonce db-atom (atom nil))

(defn connect! []
  (reset! db-atom
          (dat/init! :dat.db/datalevin schema/schema
                     {:dir (config/get :db-dir)})))

(defn db []
  (if (nil? @db-atom)
    (connect!)
    @db-atom))

(defn remove-nil-vals [m]
  (->> m
       (filter (fn [[_k v]]
                 (some? v)))
       (into {})))

(defn transact! [txs]
  (dat/transact! (db) txs))

(defn add-sim!
  [island-id sim]
  (transact!
    [{:island/id island-id
      :island/sims [sim]}]))

(defn q [query & args]
  (apply dat/q query @(db) args))

#_(connect!)

;; all
#_(q '[:find [?e ...]
       :where [?e _ _]])

;; drop all
#_(dat/clear! db-atom)

;; TODO close when app closes
;; or else lock gets stuck
#_(dat/close! db-atom)

;; reset
#_(do
    (when @db-atom
      (dat/clear! db-atom))
    (connect!))



(defn retract-all! []
  (transact!
    (map (fn [e] [:db/retractEntity e])
         (q '[:find [?e ...]
              :where [?e _ _]])))
    nil)

;; WATCHERS
;; datalevin conns aren't clojure.lang.IRef, so add-watch doesn't work on them;
;; use datalevin.core/listen! instead, re-registering whenever the conn or watchers change

(defonce watchers (atom {}))

(defn watch! [k f]
  (swap! watchers assoc k f))

(defn- register-watchers! []
  (doseq [[k f] @watchers]
    ((requiring-resolve 'datalevin.core/listen!) (dat/conn (db)) k f)))

(add-watch watchers
  ::watcher-watcher
  (fn [_ _ _ _]
    (register-watchers!)))

(add-watch db-atom
  ::db-watcher
  (fn [_ _ _ _]
    (register-watchers!)))
