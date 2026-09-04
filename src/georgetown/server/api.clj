(ns georgetown.server.api
  (:require
    [clojure.string :as string]
    [bloom.commons.uuid :as uuid]
    [georgetown.server.db :as db]
    [georgetown.server.email :as email]
    [georgetown.server.state :as s]))

(defn normalize
  [s]
  (-> s
      (string/trim)
      (string/lower-case)))

(def commands
  [
   {:id :query/version
    :params {:user-id :any}
    :return
    (fn [_]
      {:version (or (System/getenv "COMMIT") "DEV")})}


   {:id :command/authenticate-user!
    :params [:map
             [:user-id {:optional true} [:maybe :uuid]]
             [:url {:optional true} :string]
             [:email :user/email]]
    :effect
    (fn [{:keys [email url]}]
      (let [user-id (or (s/email->user-id (normalize email))
                        (let [id (uuid/random)]
                          (db/transact!
                            [{:user/id id
                              :user/email (normalize email)}])
                          id))]
        (email/send! (email/auth-email {:user-id user-id
                                        :url url}))))}

   ;; temporary authentication method
   {:id :command/alpha.authenticate-user!
    :params [:map
             [:user-id {:optional true} [:maybe :uuid]]
             [:url {:optional true} :string]
             [:email :user/email]]
    :effect
    (fn [{:keys [email url]}]
      (let [user-id (or (s/email->user-id (normalize email))
                        (let [id (uuid/random)]
                          (db/transact!
                            [{:user/id id
                              :user/email (normalize email)}])
                          id))]
        {:url (email/wrap-login {:user-id user-id
                                 :url url})}))
    :return :tada/effect-return}])
