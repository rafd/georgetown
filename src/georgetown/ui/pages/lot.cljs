(ns georgetown.ui.pages.lot
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.client.state :as state]
    [georgetown.schema :as schema]))

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
  (let [lot (->> @state/lots
                 (filter (fn [lot]
                           (= id (:lot/id lot))))
                 first)
        user @state/user]
    (when lot
      (let [deed (first (:deed/_lot lot))
            improvement (first (:improvement/_lot lot))
            current-user-owner? (and
                                  (:user/id @state/user)
                                  (= (:user/id (:deed/owner deed))
                                     (:user/id @state/user)))]
        ^{:key id}
        [:div
         [:div "Lot " (:lot/x lot) "," (:lot/y lot)]
         [block {:label "Deed"}
          (if deed
            [:div {:tw "bg-#c4ad97 text-#592510"}
             [:div.owner
              "Owned by:" (:user/id (:deed/owner deed))]
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
                    [:div
                     ;; TODO demand / supply
                     [:div {:tw "border-1 p-1"}
                      [:div.offer-type
                       (:offerable/label offerable)]
                      [:div.offer-amount
                       [:input {:type "number"
                                :name "offer-amount"
                                :min 1
                                :default-value (:offer/amount offer)
                                :step 1
                                :on-change (fn [e]
                                             (state/exec!
                                               :command/set-offer!
                                               {:improvement-id (:improvement/id improvement)
                                                :offer-type (:offerable/id offerable)
                                                :offer-amount (js/parseInt (.. e -target -value))}))}]]]])
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

