(ns georgetown.core
  (:require
    [bloom.omni.core :as omni]
    [georgetown.omni-config :as omni-config]
    [georgetown.cqrs]
    [georgetown.push :as push]))

(defn start! []
  (omni/start! omni/system omni-config/omni-config)
  (push/initialize!))

(defn -main [_])

#_(start!)

