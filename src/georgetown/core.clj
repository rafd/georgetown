(ns georgetown.core
  (:require
    [bloom.omni.core :as omni]
    [georgetown.omni-config :as omni-config]))

(defn start! []
  (omni/start! omni/system omni-config/omni-config))

(defn -main [_])

#_(start!)

