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
                       :island/players]))}


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
       [#(nil? (s/->player-id user-id [:island/id island-id]))]])
    :effect
    (fn [{:keys [user-id island-id]}]
      (db/transact!
        [{:db/id -1
          :player/id (uuid/random)
          :player/money-balance 0}
         [:db/add [:island/id island-id]
          :island/players -1]
         [:db/add [:user/id user-id]
          :user/players -1]]))}

   {:id :command/buy-lot!
    :params {:user-id :user/id
             :lot-id :lot/id}
    :conditions
    (fn [{:keys [user-id lot-id]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :lot/id lot-id)]
       [#(s/->player-id user-id [:lot/id lot-id])] ;; is player on this island
       [#(not (s/owns? user-id [:lot/id lot-id]))]]
      ;; TODO check if can afford - :fn/withdraw will throw, so not urgent
      )
    :effect
    (fn [{:keys [user-id lot-id]}]
      (let [player-id (s/->player-id user-id [:lot/id lot-id])
            lot (s/by-id [:lot/id lot-id]
                         [{:lot/improvement [:improvement/type]}
                          {:lot/deed
                           [:deed/id
                            :deed/rate
                            {:player/_deeds [:player/id]}]}])
            current-epoch (s/qget [:lot/id lot-id]
                                  [:island/_lots :island/epoch])]
        (if-let [deed (:lot/deed lot)]
          (let [refund-amount (or (:blueprint/price (blueprints/blueprints (:improvement/type (:lot/improvement lot))))
                                  0)]
            (db/transact!
              [;; refund previous owner
               [:fn/deposit (:player/id (:player/_deeds deed)) refund-amount]
               ;; remove previous deed
               [:db/retractEntity [:deed/id (:deed/id deed)]]
               ;; charge new owner
               [:fn/withdraw player-id refund-amount]
               ;; create new deed
               {:db/id -1
                :deed/id (uuid/random)
                :deed/rate (inc (:deed/rate deed))
                :deed/rate-change-at current-epoch}
               [:db/add [:lot/id lot-id] :lot/deed -1]
               [:db/add [:player/id player-id] :player/deeds -1]]))
          (db/transact!
            [;; create new deed
             {:db/id -1
              :deed/id (uuid/random)
              :deed/rate 0
              :deed/rate-changed-at current-epoch}
             [:db/add [:lot/id lot-id] :lot/deed -1]
             [:db/add [:player/id player-id] :player/deeds -1]]))))}

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
       [#(s/can-afford? (s/->player-id user-id [:lot/id lot-id])
                        (:blueprint/price (blueprints/blueprints improvement-type)))]])
    :effect
    (fn [{:keys [user-id lot-id improvement-type]}]
      (let [blueprint (blueprints/blueprints improvement-type)
            amount (:blueprint/price blueprint)]
        (db/transact!
          [{:lot/id lot-id
            :lot/improvement
            {:improvement/id (uuid/random)
             :improvement/type improvement-type
             ;; offers with vars start without an amount, ie. inactive
             :improvement/offers
             (->> (:blueprint/offerables blueprint)
                  (mapv (fn [offerable]
                          {:offer/id (uuid/random)
                           :offer/type (:offerable/id offerable)})))}}
           [:fn/transfer-to-government
            (s/qget [:lot/id lot-id] [:island/_lots :island/id])
            amount]
           [:fn/withdraw
            (s/->player-id user-id [:lot/id lot-id])
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
           [:fn/deposit (s/->player-id user-id [:improvement/id improvement-id])
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
             :player-id :player/id}
    :conditions
    (fn [{:keys [user-id player-id]}]
      [[#(s/exists? :user/id user-id)]
       [#(s/exists? :player/id player-id)]
       [#(s/owns? user-id [:player/id player-id])]])
    :effect
    (fn [{:keys [user-id player-id]}]
      (let [loan-count (->> (s/by-id [:player/id player-id]
                                     [:player/loans])
                            :player/loans
                            count)
            loan (debt/next-potential-loan loan-count)]
        (db/transact!
          [[:fn/deposit player-id (:loan/amount loan)]
           {:player/id player-id
            :player/loans
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
       [#(s/can-afford? (s/->player-id user-id [:loan/id loan-id])
                        amount)]])
    :effect
    (fn [{:keys [loan-id amount]}]
      (let [loan (s/by-id [:loan/id loan-id] [:loan/amount
                                              {:player/_loans [:player/id]}])
            player-id (:player/id (:player/_loans loan))]
        (if (<= (:loan/amount loan) amount)
          (db/transact!
            [[:db/retractEntity [:loan/id loan-id]]
             [:fn/withdraw player-id (:loan/amount loan)]])
          (db/transact!
            [[:db/add [:loan/id loan-id]
              :loan/amount (- (:loan/amount loan) amount)]
             [:fn/withdraw player-id amount]]))))}])
