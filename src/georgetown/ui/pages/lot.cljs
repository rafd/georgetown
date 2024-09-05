(ns georgetown.ui.pages.lot
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.client.state :as state]
    [georgetown.schema :as schema]
    [georgetown.ui.common :as ui]
    [georgetown.ui.map :as map]))

(defn block [{:keys [label]} & content]
  [:div {:tw "border-1 relative"}
   [:h1 {:tw "bg-black text-white px-1"} label]
   (into [:div {:tw "p-1"}]
         content)])

(defn sidebar
  [lot-id]
  (let [lot (->> @state/island
                 :island/lots
                 (filter (fn [lot]
                           (= lot-id (:lot/id lot))))
                 first)]
    (when lot
      (let [deed (:lot/deed lot)
            improvement (:lot/improvement lot)
            logged-in? @state/user
            resident? @state/resident
            owner? (and
                     logged-in?
                     resident?
                     (= (:resident/id (:resident/_deeds deed))
                        (:resident/id @state/resident)))]
        ^{:key lot-id}
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
           (cond
             owner?
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
                [ui/button {:on-click (fn []
                                        (state/exec!
                                          :command/abandon!
                                          {:lot-id (:lot/id lot)}))}
                 "Abandon"])]
             resident?
             [:div {:tw "border-1 p-1"}
              [ui/button {:on-click
                          (fn []
                            (state/exec!
                              :command/buy-lot!
                              {:lot-id (:lot/id lot)}))}
               "Purchase"
               [ui/resource-amount
                (or (some-> (:deed/rate deed) inc)
                    1)
                0
                :resource/money]
               "🔁"
               ]]
             logged-in?
             [ui/join-island-button @state/island-id]
             :else
             [ui/login-button])]]
         (when owner?
           [block {:label "Improvement"}
            (if (nil? improvement)
              [block {:label "Build..."}
               [:div
                (for [blueprint (vals schema/blueprints)]
                  ^{:key (:blueprint/id blueprint)}
                  [ui/button {:on-click (fn []
                                          (state/exec!
                                            :command/build!
                                            {:lot-id (:lot/id lot)
                                             :improvement-type (:blueprint/id blueprint)}))}
                   (:blueprint/icon blueprint)
                   (:blueprint/label blueprint)
                   [ui/resource-amount (- (:blueprint/price blueprint)) 0 :resource/money]])]]
              (let [blueprint (schema/blueprints (:improvement/type improvement))]
                [:div
                 [:div
                  (:blueprint/icon (schema/blueprints (:improvement/type improvement)))]
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
                       [:div.header {:tw "flex"}
                        [:div.offer-type {:tw "grow"}
                         (:offerable/label offerable)]
                        [:div.utilization
                         (Math/round (* (:offer/utilization offer) 100)) "%"]]
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
                           0
                           (a-unit-key offerable)
                           (b-unit-key offerable)])]]))
                  [ui/button {:on-click
                              (fn []
                                (state/exec!
                                  :command/demolish!
                                  {:improvement-id (:improvement/id improvement)}))}
                   "Demolish"
                   [ui/resource-amount
                    (/ (:blueprint/price blueprint) 2)
                    0
                    :resource/money]]]]))])]))))

(defn page
  [[_ {:keys [lot-id]}]]
  [map/page-wrapper
   [sidebar lot-id]])

(pages/register-page!
  {:page/id :page/lot
   :page/view #'page
   :page/path "/island/:island-id/lot/:lot-id"
   :page/parameters {:island-id :uuid
                     :lot-id :uuid}
   :page/on-enter! (fn [[_ {:keys [island-id]}]]
                     (state/set-island-id! island-id))})

