(ns georgetown.core
  (:gen-class)
  (:require
    [bloom.omni.core :as omni]
    [georgetown.server.omni-config :as omni-config]
    [georgetown.server.push :as push]
    [georgetown.server.scheduler :as scheduler]
    [georgetown.server.tada]))

(defn start! []
  (omni/start! omni/system omni-config/omni-config)
  (scheduler/initialize!)
  (push/initialize!)
  nil)

(defn -main []
  (start!))

#_(start!)
