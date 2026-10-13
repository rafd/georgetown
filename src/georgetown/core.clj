(ns georgetown.core
  (:gen-class)
  (:require
    [bloom.omni.core :as omni]
    [georgetown.server.omni-config :as omni-config]
    [georgetown.server.push :as push]
    [georgetown.server.db :as db]
    [georgetown.server.scheduler :as scheduler]
    [georgetown.server.state :as state]
    [georgetown.server.tada]
    [georgetown.server.telemetry :as telemetry]))

(defn start! []
  (telemetry/initialize!)
  (omni/start! omni/system omni-config/omni-config)
  ;; datascript dumps exclude tx fns
  (when (db/datascript? (db/db))
    (state/register-functions! (db/db)))
  (scheduler/initialize!)
  (push/initialize!)
  nil)

(defn -main []
  (start!))

#_(start!)
