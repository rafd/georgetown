(ns georgetown.cqrs
  (:require
    [clojure.string :as string]
    [bloom.commons.uuid :as uuid]
    [tada.events.malli :as tada]
    [georgetown.email :as email]
    [georgetown.schema :as schema]
    [georgetown.state :as s]
    [georgetown.db :as db]))

(defn normalize
  [s]
  (-> s
      (string/trim)
      (string/lower-case)))

(def cqrs
  [
   {:id :query/islands
    :params {:user-id :any}
    :return
    (fn [_]
      (s/islands [:island/id]))}

   {:id :command/authenticate-user!
    :params [:map
             [:user-id {:optional true} nil?]
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

   {:id :command/immigrate!
    :params {:user-id :user/id
             :island-id :island/id}
    :conditions
    (fn [{:keys [user-id lot-id island-id]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :lot/id lot-id)]
       [#(nil? (s/->resident-id user-id [:island/id island-id]))]])
    :effect
    (fn [{:keys [user-id island-id]}]
      (db/transact!
        [{:db/id -1
          :resident/id (uuid/random)
          :resident/money-balance 1000}
         [:db/add [:island/id island-id]
          :island/residents -1]
         [:db/add [:user/id user-id]
          :user/residents -1]]))}

   {:id :command/buy-lot!
    :params {:user-id :user/id
             :lot-id :lot/id}
    :conditions
    (fn [{:keys [user-id lot-id]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :lot/id lot-id)]
       [#(s/->resident-id user-id [:lot/id lot-id])] ;; is resident on this island
       [#(not (s/owns? user-id lot-id))]]
      ;; TODO check if can afford
      )
    :effect
    (fn [{:keys [user-id lot-id]}]
      (let [resident-id (s/->resident-id user-id [:lot/id lot-id])
            lot (s/by-id [:lot/id lot-id]
                         [{:lot/improvement [:improvement/type]}
                          {:lot/deed
                           [:deed/id
                            :deed/rate
                            {:resident/_deeds [:resident/id]}]}])]
        (if-let [deed (:lot/deed lot)]
          (let [refund-amount (:blueprint/price (schema/blueprints (:improvement/type (:lot/improvement lot))))]
            (db/transact!
              [;; refund previous owner
               [:fn/deposit (:resident/id (:resident/_deeds deed)) refund-amount]
               ;; remove previous deed
               [:db/retractEntity [:deed/id (:deed/id deed)]]
               ;; charge new owner
               [:fn/withdraw resident-id refund-amount]
               ;; create new deed
               {:db/id -1
                :deed/id (uuid/random)
                :deed/rate (inc (:deed/rate deed))}
               [:db/add [:lot/id lot-id] :lot/deed -1]
               [:db/add [:resident/id resident-id] :resident/deeds -1]]))
          (db/transact!
            [;; create new deed
             {:db/id -1
              :deed/id (uuid/random)
              :deed/rate 1}
             [:db/add [:lot/id lot-id] :lot/deed -1]
             [:db/add [:resident/id resident-id] :resident/deeds -1]]))))}

   {:id :command/change-rate!
    :params {:user-id :user/id
             :lot-id :lot/id
             :rate :deed/rate}
    :conditions
    (fn [{:keys [user-id lot-id]}]
      [[#(s/owns? user-id lot-id)]])
    :effect
    (fn [{:keys [lot-id rate]}]
      (let [d (s/lot-deed lot-id)]
        (db/transact!
          [[:db/add [:deed/id (:deed/id d)] :deed/rate rate]])))}

   {:id :command/abandon!
    :params {:user-id :user/id
             :lot-id :lot/id}
    :conditions
    (fn [{:keys [user-id lot-id]}]
      [[#(s/owns? user-id lot-id)]
       [#(nil? (s/lot-improvement lot-id))]])
    :effect
    (fn [{:keys [lot-id]}]
      (db/transact!
        [[:db/retractEntity [:deed/id (:deed/id (s/lot-deed lot-id))]]]))}

   {:id :command/build!
    :params {:user-id :user/id
             :lot-id :lot/id
             :improvement-type :improvement/type}
    :conditions
    (fn [{:keys [user-id lot-id improvement-type]}]
      [[#(s/owns? user-id lot-id)]
       [#(nil? (s/lot-improvement lot-id))]
       [#(contains? schema/blueprints improvement-type)]
       [#(s/can-afford? (s/->resident-id user-id [:lot/id lot-id])
                        (:blueprint/price (schema/blueprints improvement-type)))]])
    :effect
    (fn [{:keys [user-id lot-id improvement-type]}]
      (db/transact!
        [{:lot/id lot-id
          :lot/improvement
          {:improvement/id (uuid/random)
           :improvement/type improvement-type}}
         [:fn/withdraw
          (s/->resident-id user-id [:lot/id lot-id])
          (:blueprint/price (schema/blueprints improvement-type))]]))}

   {:id :command/demolish!
    :params {:user-id :user/id
             :improvement-id :improvement/id}
    :conditions
    (fn [{:keys [user-id improvement-id]}]
      (let [lot (s/improvement-lot improvement-id)]
        [[#(s/owns? user-id (:lot/id lot))]]))
    :effect
    (fn [{:keys [user-id improvement-id]}]
      (let [improvement (s/by-id [:improvement/id improvement-id] [:improvement/type])]
        (db/transact!
          [[:db/retractEntity [:improvement/id improvement-id]]
           [:fn/deposit (s/->resident-id user-id [:improvement/id improvement-id])
            (/ (:blueprint/price (schema/blueprints (:improvement/type improvement)))
               2)]])))}

   {:id :command/set-offer!
    :params {:user-id :user/id
             :improvement-id :improvement/id
             :offer-type :offer/type
             :offer-amount :offer/amount}
    #_#_:conditions
    (fn [{:keys [user-id improvement-id offer-type offer-amount]}]
      ;; TODO
      )
    :effect
    (fn [{:keys [improvement-id offer-type offer-amount]}]
      (db/transact!
        [{:improvement/id improvement-id
          :improvement/offers
          [{:offer/id [improvement-id offer-type]
            :offer/type offer-type
            :offer/amount offer-amount}]}]))}])

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

