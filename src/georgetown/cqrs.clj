(ns georgetown.cqrs
  (:require
    [bloom.commons.uuid :as uuid]
    [tada.events.malli :as tada]
    [georgetown.schema :as schema]
    [georgetown.state :as s]
    [georgetown.db :as db]))

(def cqrs
  [
   {:id :command/create-user!
    :params {:id :user/id}
    :conditions
    (fn [{:keys [id]}]
      [[#(not (s/exists? :user/id id))]])
    :effect
    (fn [{:keys [id]}]
      (db/transact!
        [{:user/id id}]))}

   {:id :command/immigrate!
    :params {:user-id :user/id
             :island-id :island/id}
    :conditions
    (fn [{:keys [user-id lot-id]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :lot/id lot-id)]
       ;; TODO not already a resident
       ])
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
       #_[#(not (s/owns? user-id lot-id))]])
    :effect
    (fn [{:keys [user-id lot-id]}]
      (let [owned? (s/lot-deed lot-id)]
        ;; TODO
        #_(when owned?
          (db/transact!

            )
          )
        (let [resident (s/resident user-id (:island/id (s/lot-island lot-id)))
              rate (if owned?
                     (inc (:deed/rate (s/lot-deed lot-id)))
                     1)]
          (db/transact!
            [#_[:db/cas
              [:resident/id resident-id]
              :resident/money-balance
              previous-balance
              (- previous-balance rate)]
             {:db/id -1
              :deed/id (uuid/random)
              :deed/rate rate}
             [:db/add [:lot/id lot-id] :lot/deed -1]
             [:db/add [:resident/id (:resident/id resident)] :resident/deeds -1]]))))}

   {:id :command/change-rate!
    :params {:user-id :user/id
             :lot-id :lot/id
             :rate :deed/rate}
    :conditions
    (fn [{:keys [user-id lot-id rate]}]
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

