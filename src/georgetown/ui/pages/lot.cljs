(ns georgetown.ui.pages.lot
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.client.state :as state]
    [georgetown.schema :as schema]
    [georgetown.ui.common :as ui]))

(defn block [{:keys [label]} & content]
  [:div {:tw "border-1 relative"}
   [:h1 {:tw "bg-black text-white px-1"} label]
   (into [:div {:tw "p-1"}]
         content)])

(defn button [opts & content]
  [:button (assoc opts
             :tw "bg-gray-500 text-white px-1")
   (into [:<>]
         content)])

(defn view
  [[_ {:keys [id]}]]
  (let [lot (->> @state/island
                 :island/lots
                 (filter (fn [lot]
                           (= id (:lot/id lot))))
                 first)]
    (when lot
      (let [deed (:lot/deed lot)
            improvement (:lot/improvement lot)
            current-user-owner? (and
                                  (:resident/id @state/resident)
                                  (= (:resident/id (:resident/_deeds deed))
                                     (:resident/id @state/resident)))]
        ^{:key id}
        [:div
         [:div "Lot " (:lot/x lot) "," (:lot/y lot)]
         [block {:label "Deed"}
          (if deed
            [:div {:tw "bg-#c4ad97 text-#592510"}
             [:div.owner
              "Owned by:"
              (:user/id (:user/_residents (:resident/_deeds deed)))]
             [:div.rate
              "Rate:" (:deed/rate deed)]]
            [:div "Unowned"])
          [block {:label "Actions"}
           (if current-user-owner?
             [:<>
              [:div {:tw "border-1 p-1"}
               "Rate:"
               [:input {:type "number"
                        :name "rate"
                        :min 1
                        :default-value
                        (or (:deed/rate deed) 1)
                        :on-change (fn [e]
                                     (state/exec!
                                      :command/change-rate!
                                      {:lot-id (:lot/id lot)
                                       :rate (js/parseInt (.. e -target -value))}))
                        :step 1}]]
              (when (nil? improvement)
                [button {:on-click (fn []
                                      (state/exec!
                                        :command/abandon!
                                        {:lot-id (:lot/id lot)}))}
                 "Abandon"])]
             [:div {:tw "border-1 p-1"}
              [button {:on-click
                       (fn []
                         (state/exec!
                           :command/buy-lot!
                           {:lot-id (:lot/id lot)}))}
               "Purchase"
               " ("
               (or (some-> (:deed/rate deed) inc)
                   1) "💲🔁"
               ")"
               ]])]]
         (when current-user-owner?
           [block {:label "Improvement"}
            (if (nil? improvement)
              [block {:label "Build..."}
               [:div
                (for [blueprint (vals schema/blueprints)]
                  ^{:key (:blueprint/id blueprint)}
                  [button {:on-click (fn []
                                       (state/exec!
                                         :command/build!
                                         {:lot-id (:lot/id lot)
                                          :improvement-type (:blueprint/id blueprint)}))}
                   (:blueprint/icon blueprint)
                   (:blueprint/label blueprint)])]]
              (let [blueprint (schema/blueprints (:improvement/type improvement))]
                [:div
                 (case (:improvement/type improvement)
                   :improvement.type/house
                   [:div "🏠"]
                   :improvement.type/farm
                   [:div "🌽"])
                 [:div.action
                  (doall
                    (for [offerable (:blueprint/offerables blueprint)
                          :let [offer (->> @state/offers
                                           (filter
                                             (fn [offer]
                                               (and
                                                 (= (first (:offer/id offer))
                                                    (:improvement/id improvement))
                                                 (= (:offer/type offer)
                                                    (:offerable/id offerable)))))
                                           first)]]
                      ^{:key (:offerable/id offerable)}
                      [:div {:tw "border-1 p-1"}
                       [:div.offer-type
                        (:offerable/label offerable)]
                       [:div {:tw "flex gap-2 items-center"}
                        (->> (for [[amount-key unit-key] [[:offerable/supply-amount :offerable/supply-unit]
                                                          [:offerable/demand-amount :offerable/demand-unit]]
                                   :let [amount (offerable amount-key)
                                         unit (offerable unit-key)]]
                               ^{:key unit-key}
                               [:div {:tw "flex items-center gap-1 bg-green-200 rounded p-2"}
                                (or amount
                                    [:div.offer-amount
                                     [:input {:type "number"
                                              :tw "border p-1 w-18 -m-1"
                                              :name "offer-amount"
                                              :min 1
                                              :default-value (:offer/amount offer)
                                              :step 1
                                              :on-change (fn [e]
                                                           (state/exec!
                                                             :command/set-offer!
                                                             {:improvement-id (:improvement/id improvement)
                                                              :offer-type (:offerable/id offerable)
                                                              :offer-amount (js/parseInt (.. e -target -value))}))}]])
                                [ui/resource-icon unit]])
                             (interpose
                               ^{:key "<>"}
                               [:div "<>"]))
                        (let [[[a-unit-key a-amount-key]
                               [b-unit-key b-amount-key]] ((if (:offerable/invert? offerable)
                                                             reverse
                                                             identity)
                                                           [[:offerable/demand-unit :offerable/demand-amount]
                                                            [:offerable/supply-unit :offerable/supply-amount]])]
                          [ui/resource-amount
                           (/ (or (a-amount-key offerable)
                                  (:offer/amount offer))
                              (or (b-amount-key offerable)
                                  (:offer/amount offer)))
                           (a-unit-key offerable)
                           (b-unit-key offerable)])]]))
                  [button {:on-click
                           (fn []
                             (state/exec!
                               :command/demolish!
                               {:improvement-id (:improvement/id improvement)}))}
                   "Demolish"]]]))])]))))

(pages/register-page!
  {:page/id :page/lot
   :page/view #'view
   :page/path "/lot/:id"
   :page/parameters {:id :uuid}})

