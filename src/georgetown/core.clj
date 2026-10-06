(ns georgetown.core
  (:gen-class)
  (:require
    [bloom.omni.core :as omni]
    [georgetown.server.omni-config :as omni-config]
    [georgetown.server.push :as push]
    [georgetown.server.scheduler :as scheduler]
    [georgetown.server.tada]
    [georgetown.server.telemetry :as telemetry]))

(defn start! []
  (telemetry/initialize!)
  (omni/start! omni/system omni-config/omni-config)
  (scheduler/initialize!)
  (push/initialize!)
  nil)

(defn -main []
  (start!))

#_(start!)
