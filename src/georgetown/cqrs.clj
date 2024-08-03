(ns georgetown.cqrs
  (:require
    [tada.events.malli :as tada]
    [georgetown.state :as s]))

(def cqrs
  [
   {:id :query/all
    :params {:user-id :user/id}
    :return
    (fn [{:keys [user-id]}]
      (s/client-state))}

   {:id :command/create-user!
    :params {:id :user/id}
    :conditions
    (fn [{:keys [id]}]
      ;; TODO doesn't exist
      )
    :effect
    (fn [{:keys [id]}]
      (s/create-user! id))}

   {:id :command/buy-lot!
    :params {:user-id :user/id
             :lot-id :lot/id
             :rate :deed/rate}
    :conditions
    (fn [{:keys [user-id lot-id]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :lot/id lot-id)]
       [#(not (s/owns? user-id lot-id))]])
    :effect
    (fn [{:keys [user-id lot-id rate]}]
      (cond
        (nil? (s/deed lot-id))
        (s/assign! user-id lot-id rate)

        (<= (inc (:deed/rate (s/deed lot-id))) rate)
        (do
          (s/refund! lot-id)
          (s/assign! user-id lot-id rate))

        :else
        nil))}

   {:id :command/change-rate!
    :params {:user-id :user/id
             :lot-id :lot/id
             :rate :deed/rate}
    :conditions
    (fn [{:keys [user-id lot-id rate]}]
      [[#(s/owns? user-id lot-id)]])
    :effect
    (fn [{:keys [user-id lot-id rate]}]
      (s/set-rate! lot-id rate))}

   {:id :command/abandon!
    :params {:user-id :user/id
             :lot-id :lot/id}
    :conditions
    (fn [{:keys [user-id lot-id]}]
      [[#(s/owns? user-id lot-id)]])
    :effect
    (fn [{:keys [user-id lot-id]}]
      ;; TODO
      )}

   {:id :command/build!
    :params {:user-id :user/id
             :lot-id :lot/id
             :improvement-type :improvement/type}
    :conditions
    (fn [{:keys [user-id lot-id improvement-type]}]
      [[#(s/owns? user-id lot-id)]
       [#(nil? (s/improvement lot-id))]])
    ;; TODO check that the improvement type is valid
    :effect
    (fn [{:keys [lot-id improvement-type]}]
      (s/build! lot-id improvement-type))}

   {:id :command/demolish!
    :params {:user-id :user/id
             :improvement-id :improvement/id}
    :conditions
    (fn [{:keys [user-id improvement-id]}]
      ;; TODO
      )
    :effect
    (fn [{:keys [user-id improvement-id]}]
      ;; TODO
      )}

   {:id :command/set-offer!
    :params {:user-id :user/id
             :improvement-id :improvement/id
             :offer-type :offer/type
             :offer-amount :offer/amount}
    :conditions
    (fn [{:keys [user-id improvement-id offer-type offer-amount]}]
      ;; TODO
      )
    :effect
    (fn [{:keys [improvement-id offer-type offer-amount]}]
      (s/set-offer! improvement-id offer-type offer-amount))}])

(tada/register! cqrs)

(defn exec! [k params]
  (tap> ["exec!" k params])
  (try
    (tada/do! k params)
    (catch clojure.lang.ExceptionInfo e
      (println
        (ex-message e))
      (clojure.pprint/pprint
        (ex-data e))
      (throw e))))

#_(exec! :query/all
         {:user-id #uuid "00000000-0000-0000-0000-000000000000"})
