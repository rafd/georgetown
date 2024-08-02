(ns georgetown.omni-config
  (:require
    [georgetown.config :as config]
    [georgetown.routes :as routes]))

(def omni-config
  {:omni/http-port (config/get :http-port)
   :omni/title "georgetown"
   :omni/environment (config/get :environment)
   :omni/cljs {:main "georgetown.client.core"}
   :omni/css {:tailwind? true}
   :omni/auth {:cookie {:name "georgetown"
                        :secret (config/get :auth-cookie-secret)}
               :token {:secret (config/get :auth-token-secret)}}
   :omni/api-routes #'routes/api})
