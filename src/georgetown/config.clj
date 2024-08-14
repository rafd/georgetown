(ns georgetown.config
  (:refer-clojure :exclude [get])
  (:require
    [bloom.commons.config :as config]))

(def schema
  [:map
   [:http-port integer?]
   [:environment [:enum :dev :prod]]
   [:website-base-url string?]
   [:auth-cookie-secret string?]
   [:auth-token-secret string?]
   [:smtp-credentials
    {:optional true}
    [:map
     [:port integer?]
     [:host string?]
     [:tls boolean?]
     [:from string?]
     [:user string?]
     [:pass string?]]]])

(def config
  (delay (config/read "config.edn" schema)))

(defn get [k]
  (k @config))

