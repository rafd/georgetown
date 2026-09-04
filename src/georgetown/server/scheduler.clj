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

(defn initialize!
  []
  (when @scheduler
    (.close @scheduler))
  (reset! scheduler
            (chime/chime-at
              (chime/periodic-seq (Instant/now)
                                  (Duration/ofSeconds 2))
              (fn [_time]
                (tick-all!))
              {:on-finished (fn []
                              (tap> "Schedule finished."))}))
  nil)
