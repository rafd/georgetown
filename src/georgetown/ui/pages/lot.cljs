(ns georgetown.ui.pages.lot
  (:require
    [bloom.commons.fontawesome :as fa]
    [bloom.commons.debounce :as debounce]
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

(defn offer-amount-view
  [{:keys [offer-amount improvement-id offerable-id]}]
  [:div.offer-amount
   [:input {:type "number"
            :tw "border p-1 w-18 -m-1 bg-yellow-100 rounded text-right tabular-nums"
            :name "offer-amount"
            :min 1
            :default-value offer-amount
            :step 1
            :on-change (debounce/debounce
                         (fn [e]
                           (state/exec!
                             :command/set-offer!
                             {:improvement-id improvement-id
                              :offer-type offerable-id
                              :offer-amount (js/parseInt (.. e -target -value))}))
                         250)}]])

(defn deed-rate-view
  [{:keys [lot-id deed-rate]}]
  [:div {:tw "border-1 p-1"}
   [ui/label-with-info
    "Land Tax Rate"
    "self assessed land tax rate; another resident may acquire your lot by paying a higher rate"]
   [:div {:tw "flex items-center gap-1 bg-gray-200 rounded p-2"}
    [:input {:type "number"
             :tw "border p-1 bg-yellow-100 rounded text-right max-w-5em"
             :name "rate"
             :min 0
             :default-value deed-rate
             :on-change (debounce/debounce
                          (fn [e]
                            (state/exec!
                              :command/change-rate!
                              {:lot-id lot-id
                               :rate (js/parseInt (.. e -target -value))}))
                          250)
             :step 1}]
    [ui/resource-icons [:resource/money :resource/time]]]])

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
              [deed-rate-view {:lot-id (:lot/id lot)
                               :deed-rate (or (:deed/rate deed) 0)}]

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
               "("
               (when improvement
                 [:<>
                  [ui/resource-amount
                   (:blueprint/price (schema/blueprints (:improvement/type improvement)))
                   0
                   :resource/money]
                  "+"])
               [ui/resource-amount
                (or (some-> (:deed/rate deed) inc)
                    0)
                0
                (list
                  :resource/money
                  :resource/time)]
               ")"]]
             logged-in?
             [ui/join-island-button @state/island-id]
             :else
             [ui/login-button])]]
         (when owner?
           [block {:label "Improvement"}
            (if (nil? improvement)
              [block {:label "Build..."}
               [:div {:tw "space-y-2"}
                (for [blueprint (vals schema/blueprints)]
                  ^{:key (:blueprint/id blueprint)}
                  [:div {:tw "bg-gray-200 rounded p-1 flex justify-between items-center gap-1"}
                   [:div {:tw "text-3xl"} (:blueprint/icon blueprint)]
                   [:div {:tw "grow"}
                    [:div (:blueprint/label blueprint)]
                    [:div {:tw "text-xs"} (:blueprint/description blueprint)]
                    (let [grouped-ios (->> (:blueprint/io blueprint)
                                           (group-by :io/direction))]
                      [:div {:tw "flex gap-1"}
                       (for [io (:io.direction/input grouped-ios)]
                         ^{:key (hash io)}
                         [:div
                          [ui/resource-amount
                           (:io/amount io)
                           0
                           (:io/resource io)]])
                       (when (seq (:io.direction/input grouped-ios))
                         [:span "⇒"])
                       (for [io (:io.direction/output grouped-ios)]
                         ^{:key (hash io)}
                         [:div
                          [ui/resource-amount
                           (:io/amount io)
                           0
                           (:io/resource io)]])])]
                   [ui/button {:disabled (< @state/money-balance (:blueprint/price blueprint))
                               :on-click (fn []
                                           (state/exec!
                                             :command/build!
                                             {:lot-id (:lot/id lot)
                                              :improvement-type (:blueprint/id blueprint)}))}
                     "Build"
                     " ("
                     [ui/resource-amount (- (:blueprint/price blueprint)) 0 :resource/money]
                     ")"]])]]
              (let [blueprint (schema/blueprints (:improvement/type improvement))]
                [:div
                 [:div
                  (:blueprint/icon (schema/blueprints (:improvement/type improvement)))]
                 [:div.action
                  (doall
                    (for [offerable (->> (:blueprint/offerables blueprint)
                                         (sort-by :offerable/prerequisite?)
                                         reverse)
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
                               [:div {:tw "flex items-center gap-1 bg-gray-200 rounded p-2"}
                                (or amount
                                    [offer-amount-view
                                     {:offer-amount (:offer/amount offer)
                                      :offerable-id (:offerable/id offerable)
                                      :improvement-id (:improvement/id improvement)}])
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
                           2
                           (list
                             (a-unit-key offerable)
                             (b-unit-key offerable))])]]))
                  [ui/button {:on-click
                              (fn []
                                (state/exec!
                                  :command/demolish!
                                  {:improvement-id (:improvement/id improvement)}))}
                   "Demolish"
                   " (+"
                   [ui/resource-amount
                    (/ (:blueprint/price blueprint) 2)
                    0
                    :resource/money]
                   ")"]]]))])]))))

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

