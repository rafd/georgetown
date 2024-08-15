(ns georgetown.core
  (:gen-class)
  (:require
    [bloom.omni.core :as omni]
    [georgetown.omni-config :as omni-config]
    [georgetown.loop :as loop]
    [georgetown.cqrs]
    [georgetown.push :as push]))

(defn start! []
  (omni/start! omni/system omni-config/omni-config)
  (loop/initialize!)
  (push/initialize!)
  nil)

(defn -main []
  (start!))

#_(start!)

