(ns georgetown.sim.api
  (:require
    [bloom.commons.uuid :as uuid]
    [event.render :as-alias render]
    [georgetown.server.db :as db]
    [georgetown.server.events :as events]
    [georgetown.server.state :as s]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.debt :as debt]
    [georgetown.sim.time :as time]))

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
      (let [player-id (uuid/random)]
        (db/transact!
          (concat
            [{:db/id -1
              :player/id player-id
              :player/money-balance 0}
             [:db/add [:island/id island-id]
              :island/players -1]
             [:db/add [:user/id user-id]
              :user/players -1]]
            (events/event-txs island-id
                              {:event/type :event.type/player-joined
                               :event/render [[::render/player {:player-id player-id}]
                                              " joined the island"]})))))}

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
                         [:lot/x
                          :lot/y
                          {:lot/improvement [:improvement/type]}
                          {:lot/deed
                           [:deed/id
                            :deed/rate
                            {:player/_deeds [:player/id]}]}])
            current-epoch (s/qget [:lot/id lot-id]
                                  [:island/_lots :island/epoch])
            island-id (s/qget [:lot/id lot-id]
                              [:island/_lots :island/id])
            deed (:lot/deed lot)
            purchase-event-txs
            (events/event-txs island-id
                              {:event/type :event.type/lot-purchased
                               ;; into, not concat: the stored value must be a vector
                               :event/render (cond-> [[::render/player {:player-id player-id}]
                                                      " bought "
                                                      [::render/lot {:lot-id lot-id
                                                                     :lot-x (:lot/x lot)
                                                                     :lot-y (:lot/y lot)}]]
                                               deed
                                               (into [" from "
                                                      [::render/player
                                                       {:player-id (:player/id (:player/_deeds deed))}]]))})]
        (if deed
          (let [refund-amount (or (:blueprint/price (blueprints/blueprints (:improvement/type (:lot/improvement lot))))
                                  0)]
            (db/transact!
              (concat
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
                  :deed/rate-changed-at current-epoch}
                 [:db/add [:lot/id lot-id] :lot/deed -1]
                 [:db/add [:player/id player-id] :player/deeds -1]]
                purchase-event-txs)))
          (db/transact!
            (concat
              [;; create new deed
               {:db/id -1
                :deed/id (uuid/random)
                :deed/rate 0
                :deed/rate-changed-at current-epoch}
               [:db/add [:lot/id lot-id] :lot/deed -1]
               [:db/add [:player/id player-id] :player/deeds -1]]
              purchase-event-txs)))))}

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
               (let [current-epoch (s/qget [:deed/id deed-id]
                                           [:lot/_deed
                                            :island/_lots
                                            :island/epoch])]
                 (not (:locked? (time/deed-rate-lock changed-at current-epoch))))))]])
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
               current-epoch (s/qget [:deed/id deed-id] [:lot/_deed :island/_lots :island/epoch])]
           (not (:locked? (time/deed-rate-lock changed-at current-epoch))))]])
    :effect
    (fn [{:keys [deed-id]}]
      (let [island-id (s/qget [:deed/id deed-id]
                              [:lot/_deed :island/_lots :island/id])
            lot-id (s/qget [:deed/id deed-id] [:lot/_deed :lot/id])
            lot-x (s/qget [:deed/id deed-id] [:lot/_deed :lot/x])
            lot-y (s/qget [:deed/id deed-id] [:lot/_deed :lot/y])]
        (db/transact!
          (concat
            [[:db/retractEntity [:deed/id deed-id]]]
            (events/event-txs island-id
                              {:event/type :event.type/lot-abandoned
                               :event/render [[::render/lot {:lot-id lot-id
                                                             :lot-x lot-x
                                                             :lot-y lot-y}]
                                              " was abandoned"]})))))}

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
            amount (:blueprint/price blueprint)
            island-id (s/qget [:lot/id lot-id] [:island/_lots :island/id])
            player-id (s/->player-id user-id [:lot/id lot-id])
            lot (s/by-id [:lot/id lot-id] [:lot/x :lot/y])]
        (db/transact!
          (concat
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
             [:fn/transfer-to-government island-id amount]
             [:fn/withdraw player-id amount]]
            (events/event-txs island-id
                              {:event/type :event.type/built
                               :event/render [[::render/player {:player-id player-id}]
                                              " built "
                                              [::render/improvement {:improvement-type improvement-type}]
                                              " on " [::render/lot {:lot-id lot-id
                                                                    :lot-x (:lot/x lot)
                                                                    :lot-y (:lot/y lot)}]]})))))}

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
      (let [improvement (s/by-id [:improvement/id improvement-id] [:improvement/type])
            island-id (s/qget [:improvement/id improvement-id]
                              [:lot/_improvement :island/_lots :island/id])
            lot-id (s/qget [:improvement/id improvement-id] [:lot/_improvement :lot/id])
            lot-x (s/qget [:improvement/id improvement-id] [:lot/_improvement :lot/x])
            lot-y (s/qget [:improvement/id improvement-id] [:lot/_improvement :lot/y])
            ;; get back only half
            amount (/ (:blueprint/price (blueprints/blueprints (:improvement/type improvement)))
                      2)]
        (db/transact!
          (concat
            [[:db/retractEntity [:improvement/id improvement-id]]
             [:fn/transfer-to-government island-id (- amount)]
             [:fn/deposit (s/->player-id user-id [:improvement/id improvement-id])
              amount]]
            (events/event-txs island-id
                              {:event/type :event.type/demolished
                               :event/render [[::render/improvement
                                               {:improvement-type (:improvement/type improvement)}]
                                              " on " [::render/lot {:lot-id lot-id
                                                                    :lot-x lot-x
                                                                    :lot-y lot-y}]
                                              " was demolished"]})))))}

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
    (fn [{:keys [player-id]}]
      (let [loan-count (->> (s/by-id [:player/id player-id]
                                     [:player/loans])
                            :player/loans
                            count)
            loan (debt/next-potential-loan loan-count)
            island-id (s/qget [:player/id player-id]
                              [:island/_players :island/id])]
        (db/transact!
          (concat
            [[:fn/deposit player-id (:loan/amount loan)]
             {:player/id player-id
              :player/loans
              [(assoc loan
                 :loan/id (uuid/random))]}]
            (events/event-txs island-id
                              {:event/type :event.type/loan-borrowed
                               :event/visibility :visibility/limited
                               :event/visibility-player-ids #{player-id}
                               :event/render ["You borrowed " [::render/money {:amount (:loan/amount loan)}]
                                              " from the bank"]})))))}

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
          (let [island-id (s/qget [:loan/id loan-id]
                                  [:player/_loans :island/_players :island/id])]
            (db/transact!
              (concat
                [[:db/retractEntity [:loan/id loan-id]]
                 [:fn/withdraw player-id (:loan/amount loan)]]
                (events/event-txs island-id
                                  {:event/type :event.type/loan-paid-off
                                   :event/visibility :visibility/limited
                                   :event/visibility-player-ids #{player-id}
                                   :event/render ["You paid off a loan"]}))))
          (db/transact!
            [[:db/add [:loan/id loan-id]
              :loan/amount (- (:loan/amount loan) amount)]
             [:fn/withdraw player-id amount]]))))}])
