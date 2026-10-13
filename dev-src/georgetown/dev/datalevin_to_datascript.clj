(ns georgetown.dev.datalevin-to-datascript
  ;; converts a datalevin db dir to the datascript dump format
  ;; run: lein run -m georgetown.dev.datalevin-to-datascript [datalevin-dir] [dump-file]
  (:require
    [datalevin.core :as datalevin]
    [datascript.db]
    [georgetown.server.db :as db]))

;; datalevin returns :db.type/float values as Floats; the app writes doubles.
;; parsing the string keeps the shortest decimal (0.1f => 0.1, not 0.10000000149)
(defn float->double [v]
  (if (instance? Float v)
    (Double/parseDouble (str v))
    v))

(defn convert!
  [{:keys [datalevin-dir dump-file]}]
  (let [conn (datalevin/get-conn datalevin-dir)]
    (try
      (let [datom-vectors (->> (datalevin/datoms (datalevin/db conn) :eav)
                               (remove (fn [datom]
                                         (= :db/fn (:a datom))))
                               ;; datalevin keeps no tx ids
                               (mapv (fn [datom]
                                       [(:e datom)
                                        (:a datom)
                                        (float->double (:v datom))
                                        datascript.db/tx0])))]
        (db/write-datascript-dump! dump-file datom-vectors)
        {:datoms (count datom-vectors)
         :dump-file dump-file})
      (finally
        (datalevin/close conn)))))

#_(convert! {:datalevin-dir "data/datalevin"
             :dump-file db/datascript-dump-file})

(defn -main
  [& [datalevin-dir dump-file]]
  (prn (convert! {:datalevin-dir (or datalevin-dir "data/datalevin")
                  :dump-file (or dump-file db/datascript-dump-file)}))
  (shutdown-agents))
