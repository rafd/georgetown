(ns georgetown.server.scheduler
  (:require
    [chime.core :as chime]
    [georgetown.server.db :as db]
    [georgetown.sim.tick :as tick])
  (:import
    [java.time Instant Duration]))

(defn tick-all! []
  (let [database (db/db)]
    (doseq [island-id (db/q database '[:find [?island-id ...]
                            :where
                            [?island :island/id ?island-id]])]
      (tick/tick! database island-id))))

#_(tick-all!)

(defonce scheduler (atom nil))

(def tick-period (Duration/ofSeconds 5))

(defn late?
  [scheduled-time]
  (-> (Duration/between scheduled-time (Instant/now))
      (.compareTo tick-period)
      (pos?)))

(defn job [scheduled-time]
  (if (late? scheduled-time)
    (tap> (str "Skipping late tick scheduled for " scheduled-time))
    (tick-all!)))

(defonce dump-scheduler (atom nil))

(def dump-period (Duration/ofSeconds 60))

(defn dump-job [_scheduled-time]
  (let [database (db/db)]
    (when (db/datascript? database)
      (db/dump-datascript! database))))

(defn initialize!
  []
  (when @scheduler
    (.close @scheduler))
  (when @dump-scheduler
    (.close @dump-scheduler))
  (reset! dump-scheduler
          (chime/chime-at
           (chime/periodic-seq (.plus (Instant/now) dump-period) dump-period)
           dump-job))
  (reset! scheduler
          (chime/chime-at
           (chime/periodic-seq (Instant/now) tick-period)
           ;; chime does not skip missed times, so a slow tick would
           ;; otherwise be followed by a burst of catch-up ticks
           job
           {:on-finished (fn []
                           (tap> "Schedule finished."))}))
  nil)

#_(initialize!)
