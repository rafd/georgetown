(ns georgetown.client.ui.pages.lot
  (:require
    [reagent.core :as r]
    [bloom.commons.debounce :as debounce]
    [bloom.commons.pages :as pages]
    [georgetown.client.state :as state]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.types :as types]
    [georgetown.client.ui.common :as ui]
    [georgetown.client.ui.map :as map]))

(defn block [{:keys [label]} & content]
  [:div {:tw "border-1 relative"}
   [:h1 {:tw "bg-black text-white px-1"} label]
   (into [:div {:tw "p-1"}]
         content)])

(def direction-labels
  {:effect.direction/from-citizen "citizen−"
   :effect.direction/to-citizen "citizen+"
   :effect.direction/from-player "you−"
   :effect.direction/to-player "you+"
   :effect.direction/from-self "stock−"
   :effect.direction/to-self "stock+"})

(defn effect-view
  [offer [direction target _ :as effect]]
  (let [amount (blueprints/resolve-effect-amount offer effect)]
    [:div {:tw "flex items-center gap-0.5 bg-gray-100 rounded px-1 text-xs whitespace-nowrap"}
     [:span {:tw "text-gray-500"} (direction-labels direction)]
     [:span (or amount "?")]
     (if (contains? types/resources target)
       [ui/resource-icon target]
       [:span {:title (:citizen-attribute/label (blueprints/citizen-attributes target))}
        (:citizen-attribute/icon (blueprints/citizen-attributes target))])]))

(defn offerable-effects-view
  [offer offerable]
  [:div {:tw "flex gap-1 flex-wrap"}
   (for [effect (:offerable/effects offerable)]
     ^{:key (hash effect)}
     [effect-view offer effect])])

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
  [{:keys [deed locked?]}]
  (r/with-let [on-change (debounce/debounce
                           (fn [e]
                             (state/exec!
                               :command/change-rate!
                               {:deed-id (:deed/id deed)
                                :rate (js/parseInt (.. e -target -value))}))
                           250)]
    [:div {:tw "border-1 p-1"}
     [ui/label-with-info
      "Land Tax Rate"
      "Self assessed land tax rate, paid per day. Another player may acquire your lot by paying a higher rate. Rate can only be decreased after 1 year."]
     [:div {:tw "flex items-center gap-1 bg-gray-200 rounded p-2"}
      [:input {:type "number"
               :tw "border p-1 bg-yellow-100 rounded text-right max-w-5em"
               :name "rate"
               :min (if locked? (:deed/rate deed) 0)
               :default-value (:deed/rate deed)
               :on-change on-change
               :step 1}]
      [ui/resource-icons [:resource/money :resource/time]]]]))

(defn abandon-button-view
  [{:keys [deed locked?]}]
  [ui/button {:disabled locked?
              :on-click (fn []
                          (state/exec!
                            :command/abandon!
                            {:deed-id (:deed/id deed)}))}
   "Abandon"])

(defn deed-actions-view
  [{:keys [deed has-improvement?]}]
  (r/with-let [now (state/subscribe [:PUBLIC :island/epoch])
               changed-at (state/subscribe [:PRIVATE :player/deeds
                                            :ALL
                                            (fn [{:deed/keys [id]}]
                                              (= id (:deed/id deed)))
                                            :deed/rate-changed-at])]
    (let [expiry (+ @changed-at 365)
          locked? (< @now expiry)]
      [:div
       (when locked?
         [:div {:tw "text-xs"}
          (str "Cannot abandon or set rate below " (:deed/rate deed) " for " (- expiry @now) " more days")])
       [deed-rate-view {:deed deed
                        :locked? locked?}]
       (when (not has-improvement?)
         [abandon-button-view {:deed deed
                               :locked? locked?}])])))

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
            player? @state/player
            owner? (and
                     logged-in?
                     player?
                     (= (:player/id (:player/_deeds deed))
                        (:player/id @state/player)))]
        ^{:key lot-id}
        [:div
         [:div "Lot " (:lot/x lot) "," (:lot/y lot)
          [:table
           [:tbody
            [:tr
             [:td "Elevation"]
             [:td (ui/format (:lot/elevation lot) 2)]]
            [:tr
             [:td "Moisture"]
             [:td (ui/format (:lot/moisture lot) 2)]]]]]
         [block {:label "Deed"}
          (if deed
            [:div {:tw "bg-#c4ad97 text-#592510"}
             [:div.owner
              "Owned by:"
              (:user/id (:user/_players (:player/_deeds deed)))]
             [:div.rate
              "Rate:" (:deed/rate deed)]]
            [:div "Unowned"])
          [block {:label "Actions"}
           (cond
             owner?
             [deed-actions-view {:deed deed
                                 :has-improvement? (boolean improvement)}]
             player?
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
                   (:blueprint/price (blueprints/blueprints (:improvement/type improvement)))
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
                (for [blueprint (vals blueprints/blueprints)]
                  ^{:key (:blueprint/id blueprint)}
                  [:div {:tw "bg-gray-200 rounded p-1 flex justify-between items-center gap-1"}
                   [:div {:tw "text-3xl"} (:blueprint/icon blueprint)]
                   [:div {:tw "grow"}
                    [:div (:blueprint/label blueprint)]
                    [:div {:tw "text-xs"} (:blueprint/description blueprint)]
                    [:div {:tw "space-y-1"}
                     (for [offerable (:blueprint/offerables blueprint)]
                       ^{:key (:offerable/id offerable)}
                       [:div {:tw "flex gap-1 items-center flex-wrap"}
                        [:span {:tw "text-xs"}
                         (:offerable/icon offerable) " "
                         (:offerable/label offerable)]
                        [offerable-effects-view nil offerable]])]]
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
              (let [blueprint (blueprints/blueprints (:improvement/type improvement))]
                [:div
                 [:div
                  (:blueprint/icon (blueprints/blueprints (:improvement/type improvement)))]
                 [:div.action
                  (doall
                    (for [offerable (:blueprint/offerables blueprint)
                          :let [offer (->> @state/offers
                                           (filter
                                             (fn [offer]
                                               (and
                                                 (= (:improvement/id (:improvement/_offers offer))
                                                    (:improvement/id improvement))
                                                 (= (:offer/type offer)
                                                    (:offerable/id offerable)))))
                                           first)]]
                      ^{:key (:offerable/id offerable)}
                      [:div {:tw "border-1 p-1 space-y-1"}
                       [:div.header {:tw "flex items-center"}
                        [:div.offer-type {:tw "grow flex items-center gap-1"}
                         [:span
                          (:offerable/icon offerable) " "
                          (:offerable/label offerable)]
                         [ui/shift-indicator (:offerable/time-shifts offerable)]]
                        (let [utilization (or (:offer/utilization offer) 0)]
                          [:div.utilization {:tw "flex items-center gap-1"
                                             :title (str (Math/round (* utilization 100)) "%")}
                           (when-let [capacity (:offerable/capacity offerable)]
                             [:span (Math/round (* utilization capacity)) " / " capacity])
                           [ui/pie {:tw "w-0.6rem h-0.6rem"
                                    :bg-color "#ddd"
                                    :fg-color "green"}
                            utilization]])]
                       (doall
                         (for [offerable-var (:offerable/var offerable)]
                           ^{:key (:var/id offerable-var)}
                           [:div {:tw "flex items-center gap-1 bg-gray-200 rounded p-2"}
                            [:span (:var/label offerable-var)]
                            [offer-amount-view
                             {:offer-amount (:offer/amount offer)
                              :offerable-id (:offerable/id offerable)
                              :improvement-id (:improvement/id improvement)}]
                            (let [[_ money-resource per-resource] (:var/unit offerable-var)]
                              [ui/resource-icons [money-resource per-resource]])]))
                       [offerable-effects-view offer offerable]]))
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

