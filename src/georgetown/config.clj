(ns georgetown.config
  (:refer-clojure :exclude [get])
  (:require
    [bloom.commons.config :as config]))

(def schema
  [:map
   [:http-port integer?]
   [:environment [:enum :dev :prod]]
   [:auth-cookie-secret string?]
   [:auth-token-secret string?]])

(def config
  (delay (config/read "config.edn" schema)))

(defn get [k]
  (k @config))

