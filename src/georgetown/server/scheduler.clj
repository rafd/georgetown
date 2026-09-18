(ns georgetown.server.scheduler
  (:require
    [chime.core :as chime]
    [georgetown.server.db :as db]
    [georgetown.sim.tick :as tick])
  (:import
    [java.time Instant Duration]))

(defn tick-all! []
  (doseq [island-id (db/q '[:find [?island-id ...]
                            :where
                            [?island :island/id ?island-id]])]
    (tick/tick! island-id)))

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

(defn initialize!
  []
  (when @scheduler
    (.close @scheduler))
  (reset! scheduler
          (chime/chime-at
           (chime/periodic-seq (Instant/now) tick-period)
           ;; chime does not skip missed times, so a slow tick would
           ;; otherwise be followed by a burst of catch-up ticks
           job
           {:on-finished (fn []
                           (tap> "Schedule finished."))}))
  nil)
