(ns georgetown.server.db
  (:require
    [clojure.java.io :as io]
    [dat.api :as dat]
    [datalevin.core :as datalevin]
    [datascript.core :as datascript]
    [taoensso.nippy :as nippy]
    [georgetown.sim.schema :as schema]
    [georgetown.server.config :as config])
  (:import
    [java.nio.file CopyOption Files StandardCopyOption]))

;; one of :datalevin, :datalevin-memory, :datascript-memory
(def backend :datascript-memory)

(defonce db-atom (atom nil))

(defn datalevin? [db]
  (= :dat.db/datalevin (:dat.api/db-type @db)))

(defn datascript? [db]
  (= :dat.db/datascript (:dat.api/db-type @db)))

;; env flags are not persisted, so must be set on every open
(defn set-env-flags!
  [db]
  (when (datalevin? db)
    (let [lmdb (-> (:dat.api/conn @db)
                   (datalevin/db)
                   (.-store)
                   (.-lmdb))]
      (datalevin/set-env-flags lmdb #{:nometasync :nosync} true))))

(defmulti init-db! (fn [backend] backend))

(defmethod init-db! :datalevin
  [_]
  (dat/init! :dat.db/datalevin schema/schema
             {:dir (config/get :db-dir)}))

(defmethod init-db! :datalevin-memory
  [_]
  (dat/init! :dat.db/datalevin schema/schema
             {:dir nil
              :opts {:kv-opts {:inmemory? true}}}))

(def datascript-dump-file "data/datascript.nippy")

(defn write-datascript-dump!
  [file datom-vectors]
  (let [tmp-file (io/file (str file ".tmp"))]
    (io/make-parents tmp-file)
    (nippy/freeze-to-file tmp-file (vec datom-vectors))
    (Files/move (.toPath tmp-file)
                (.toPath (io/file file))
                (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                        StandardCopyOption/REPLACE_EXISTING]))))

;; :db/fn values are functions, so they are not dumped;
;; re-register them after restore (state/register-functions!)
(defn dump-datascript!
  [db]
  (->> (datascript/datoms @(dat/conn db) :eavt)
       (remove (fn [datom]
                 (= :db/fn (:a datom))))
       (map (fn [datom]
              [(:e datom) (:a datom) (:v datom) (:tx datom)]))
       (write-datascript-dump! datascript-dump-file)))

(defmethod init-db! :datascript-memory
  [_]
  (let [db (dat/init! :dat.db/datascript schema/schema {})
        conn (dat/conn db)]
    (when (.exists (io/file datascript-dump-file))
      (datascript/reset-conn! conn
        (datascript/init-db
          (->> (nippy/thaw-from-file datascript-dump-file)
               (map (fn [[e a v tx]]
                      (datascript/datom e a v tx))))
          (datascript/schema @conn))))
    db))

(defn connect! []
  (doto (reset! db-atom (init-db! backend))
    (set-env-flags!)))

(defn db []
  (if (nil? @db-atom)
    (connect!)
    @db-atom))

(defn remove-nil-vals [m]
  (->> m
       (filter (fn [[_k v]]
                 (some? v)))
       (into {})))

(defn transact! [db txs]
  (dat/transact! db txs))

(defn add-citizen!
  [db island-id citizen]
  (transact! db
    [{:island/id island-id
      :island/citizens [citizen]}]))

(defn q [db query & args]
  (apply dat/q query @db args))

#_(connect!)

;; all
#_(q (db) '[:find [?e ...]
       :where [?e _ _]])

;; TODO close when app closes
;; or else lock gets stuck
#_(dat/close! db-atom)

(defn retract-all! [db]
  (transact! db
    (map (fn [e] [:db/retractEntity e])
         (q db '[:find [?e ...]
              :where [?e _ _]]))))

(defn clear! [db]
  (dat/clear! db)
  (set-env-flags! db))

;; WATCHERS
;; datalevin conns aren't clojure.lang.IRef, so add-watch doesn't work on them;
;; use dat/listen! instead, re-registering whenever the conn or watchers change

(defonce watchers (atom {}))

(defn watch! [k f]
  (swap! watchers assoc k f))

(defn register-watchers! []
  (doseq [[k f] @watchers]
    (dat/listen! (db) k f)))

(add-watch watchers
  ::watcher-watcher
  (fn [_ _ _ _]
    (register-watchers!)))

(add-watch db-atom
  ::db-watcher
  (fn [_ _ _ _]
    (register-watchers!)))
