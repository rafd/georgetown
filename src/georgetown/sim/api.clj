(ns georgetown.sim.api
  (:require
    [bloom.commons.uuid :as uuid]
    [georgetown.server.db :as db]
    [georgetown.server.state :as s]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.debt :as debt]))

(def commands
  [
   {:id :query/islands
    :params {:user-id :any}
    :return
    (fn [_]
      (s/all-of-type :island/id
                     '[:island/id
                       {:island/lots [:lot/x
                                      :lot/y
                                      :lot/elevation
                                      :lot/moisture]}
                       :island/residents]))}


   {:id :command/create-island!
    :params [:map
             [:user-id {:optional true} [:maybe :uuid]]]
    :effect
    (fn [_]
      (s/create-island!))}

   {:id :command/immigrate!
    :params {:user-id :user/id
             :island-id :island/id}
    :conditions
    (fn [{:keys [user-id island-id]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :island/id island-id)]
       [#(nil? (s/->resident-id user-id [:island/id island-id]))]])
    :effect
    (fn [{:keys [user-id island-id]}]
      (db/transact!
        [{:db/id -1
          :resident/id (uuid/random)
          :resident/money-balance 0}
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
       [#(not (s/owns? user-id [:lot/id lot-id]))]]
      ;; TODO check if can afford - :fn/withdraw will throw, so not urgent
      )
    :effect
    (fn [{:keys [user-id lot-id]}]
      (let [resident-id (s/->resident-id user-id [:lot/id lot-id])
            lot (s/by-id [:lot/id lot-id]
                         [{:lot/improvement [:improvement/type]}
                          {:lot/deed
                           [:deed/id
                            :deed/rate
                            {:resident/_deeds [:resident/id]}]}])
            current-epoch (s/qget [:lot/id lot-id]
                                  [:island/_lots :island/epoch])]
        (if-let [deed (:lot/deed lot)]
          (let [refund-amount (or (:blueprint/price (blueprints/blueprints (:improvement/type (:lot/improvement lot))))
                                  0)]
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
                :deed/rate (inc (:deed/rate deed))
                :deed/rate-change-at current-epoch}
               [:db/add [:lot/id lot-id] :lot/deed -1]
               [:db/add [:resident/id resident-id] :resident/deeds -1]]))
          (db/transact!
            [;; create new deed
             {:db/id -1
              :deed/id (uuid/random)
              :deed/rate 0
              :deed/rate-changed-at current-epoch}
             [:db/add [:lot/id lot-id] :lot/deed -1]
             [:db/add [:resident/id resident-id] :resident/deeds -1]]))))}

   {:id :command/change-rate!
    :params {:user-id :user/id
             :deed-id :deed/id
             :rate :deed/rate}
    :conditions
    (fn [{:keys [user-id deed-id rate]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :deed/id deed-id)]
       [#(s/owns? user-id [:deed/id deed-id])]
       [;; docs.lot.change-rate - when changing the tax rate, it cannot be lowered for 1 year
        #(let [{current-rate :deed/rate
                changed-at :deed/rate-changed-at}
               (s/by-id [:deed/id deed-id]
                        [:deed/rate
                         :deed/rate-changed-at])]
           (or (< current-rate rate)
               (let [expiry (+ 365 changed-at)
                     current-epoch (s/qget [:deed/id deed-id]
                                           [:lot/_deed
                                            :island/_lots
                                            :island/epoch])]
                 (< expiry current-epoch))))]])
    :effect
    (fn [{:keys [deed-id rate]}]
      (db/transact!
        [{:deed/id deed-id
          :deed/rate rate
          :deed/rate-changed-at (s/qget [:deed/id deed-id] [:lot/_deed :island/_lots :island/epoch])}]))}

   ;; docs.lot.abandon - a lot can be abandoned
   {:id :command/abandon!
    :params {:user-id :user/id
             :deed-id :deed/id}
    :conditions
    (fn [{:keys [user-id deed-id]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :deed/id deed-id)]
       [#(s/owns? user-id [:deed/id deed-id])]
       [;; docs.lot.abandon - a lot cannot be abandoned if there is still an improvement on it
        #(nil? (s/qget [:deed/id deed-id] [:lot/_deed :lot/improvement]))]
       [;; docs.lot.abandon - a lot cannot be abandoned if the rate has been changed within the last year
        #(let [changed-at (s/qget [:deed/id deed-id] [:deed/rate-changed-at])
               expiry (+ 365 changed-at)
               current-epoch (s/qget [:deed/id deed-id] [:lot/_deed :island/_lots :island/epoch])]
           (< expiry current-epoch))]])
    :effect
    (fn [{:keys [deed-id]}]
      (db/transact!
        [[:db/retractEntity [:deed/id deed-id]]]))}

   {:id :command/build!
    :params {:user-id :user/id
             :lot-id :lot/id
             :improvement-type :improvement/type}
    :conditions
    (fn [{:keys [user-id lot-id improvement-type]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :lot/id lot-id)]
       [#(contains? blueprints/blueprints improvement-type)]
       [#(s/owns? user-id [:lot/id lot-id])]
       [#(nil? (:lot/improvement (s/by-id [:lot/id lot-id] [:lot/improvement])))]
       [#(s/can-afford? (s/->resident-id user-id [:lot/id lot-id])
                        (:blueprint/price (blueprints/blueprints improvement-type)))]])
    :effect
    (fn [{:keys [user-id lot-id improvement-type]}]
      (let [amount (:blueprint/price (blueprints/blueprints improvement-type))]
        (db/transact!
          [{:lot/id lot-id
            :lot/improvement
            {:improvement/id (uuid/random)
             :improvement/type improvement-type}}
           [:fn/transfer-to-government
            (s/qget [:lot/id lot-id] [:island/_lots :island/id])
            amount]
           [:fn/withdraw
            (s/->resident-id user-id [:lot/id lot-id])
            amount]])))}

   {:id :command/demolish!
    :params {:user-id :user/id
             :improvement-id :improvement/id}
    :conditions
    (fn [{:keys [user-id improvement-id]}]
        [[#(s/exists? :user/id user-id)]
         [#(s/exists? :improvement/id improvement-id)]
         [#(s/owns? user-id [:improvement/id improvement-id])]])
    :effect
    (fn [{:keys [user-id improvement-id]}]
      (let [improvement (s/by-id [:improvement/id improvement-id] [:improvement/type])]
        (let [;; get back only half
              amount (/ (:blueprint/price (blueprints/blueprints (:improvement/type improvement)))
                        2)]
        (db/transact!
          [[:db/retractEntity [:improvement/id improvement-id]]
           [:fn/transfer-to-government
            (s/qget [:improvement/id improvement-id] [:lot/_improvement :island/_lots :island/id])
            (- amount)]
           [:fn/deposit (s/->resident-id user-id [:improvement/id improvement-id])
            amount]]))))}

   {:id :command/set-offer!
    :params {:user-id :user/id
             :improvement-id :improvement/id
             :offer-type :offer/type
             :offer-amount :offer/amount}
    :conditions
    (fn [{:keys [user-id improvement-id offer-type]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :improvement/id improvement-id)]
       [#(s/owns? user-id [:improvement/id improvement-id])]
       ;; offer-type is allowed for this improvement
       [#(->> (blueprints/blueprints (:improvement/type
                                   (s/by-id [:improvement/id improvement-id]
                                            [:improvement/type])))
              :blueprint/offerables
              (some (fn [offerable]
                        (= (:offerable/id offerable) offer-type))))]])
    :effect
    (fn [{:keys [improvement-id offer-type offer-amount]}]
      (let [?existing-offer-id (->> (s/by-id [:improvement/id improvement-id]
                                             [{:improvement/offers
                                               [:offer/id :offer/type]}])
                                    :improvement/offers
                                    (filter (fn [offer]
                                              (= offer-type (:offer/type offer))))
                                    first
                                    :offer/id)]
        (db/transact!
          [{:improvement/id improvement-id
            :improvement/offers
            [{:offer/id (or ?existing-offer-id (uuid/random))
              :offer/type offer-type
              :offer/amount offer-amount}]}])))}

   {:id :command/borrow-loan!
    :params {:user-id :user/id
             :resident-id :resident/id}
    :conditions
    (fn [{:keys [user-id resident-id]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :resident/id resident-id)]
       [#(s/owns? user-id [:resident/id resident-id])]])
    :effect
    (fn [{:keys [user-id resident-id]}]
      (let [loan-count (->> (s/by-id [:resident/id resident-id]
                                     [:resident/loans])
                            :resident/loans
                            count)
            loan (debt/next-potential-loan loan-count)]
        (db/transact!
          [[:fn/deposit resident-id (:loan/amount loan)]
           {:resident/id resident-id
            :resident/loans
            [(assoc loan
               :loan/id (uuid/random))]}])))}

   {:id :command/set-loan-daily-payment-amount!
    :params {:user-id :user/id
             :loan-id :loan/id
             :daily-payment-amount :loan/daily-payment-amount}
    :conditions
    (fn [{:keys [user-id loan-id daily-payment-amount]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :loan/id loan-id)]
       [#(s/owns? user-id [:loan/id loan-id])]
       [#(<= (:loan/minimum-daily-payment-amount
               (s/by-id [:loan/id loan-id]
                        [:loan/minimum-daily-payment-amount]))
             daily-payment-amount)]])
    :effect
    (fn [{:keys [loan-id daily-payment-amount]}]
      (db/transact!
        [[:db/add [:loan/id loan-id]
          :loan/daily-payment-amount daily-payment-amount]]))}

   {:id :command/repay-loan!
    :params {:user-id :user/id
             :loan-id :loan/id
             :amount :pos-int}
    :conditions
    (fn [{:keys [user-id loan-id amount]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :loan/id loan-id)]
       [#(s/owns? user-id [:loan/id loan-id])]
       [#(s/can-afford? (s/->resident-id user-id [:loan/id loan-id])
                        amount)]])
    :effect
    (fn [{:keys [loan-id amount]}]
      (let [loan (s/by-id [:loan/id loan-id] [:loan/amount
                                              {:resident/_loans [:resident/id]}])
            resident-id (:resident/id (:resident/_loans loan))]
        (if (<= (:loan/amount loan) amount)
          (db/transact!
            [[:db/retractEntity [:loan/id loan-id]]
             [:fn/withdraw resident-id (:loan/amount loan)]])
          (db/transact!
            [[:db/add [:loan/id loan-id]
              :loan/amount (- (:loan/amount loan) amount)]
             [:fn/withdraw resident-id amount]]))))}])
