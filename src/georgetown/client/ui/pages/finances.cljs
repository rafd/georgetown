(ns georgetown.client.ui.pages.finances
  (:require
    [bloom.commons.pages :as pages]
    [com.rpl.specter :as x]
    [georgetown.client.ui.common :as ui]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.client.state :as state]
    [georgetown.client.ui.dataviz :as dataviz]
    [georgetown.client.ui.map :as map]))

(defn cashflow-graph []
  [:div {:tw "flex justify-end p-4"}
   [dataviz/plus-minus-sparkline
    (x/select [x/ALL :stats.private/net-cashflow] @state/private-stats-history)]])

(defn offer-with-net-amount
  [offer direction]
  (let [offerable (blueprints/offerables (:offer/type offer))
        per-unit (blueprints/effect-sum offer direction :resource/money)]
    (when (pos? per-unit)
      (assoc offer :offer/net-amount
        (* (or (:offer/utilization offer) 0)
           (or (:offerable/capacity offerable) 1)
           per-unit)))))

(defn cashflow-table []
  (let [player-id (:player/id @state/player)
        improvement-id->offers (->> @state/offers
                                    (group-by (fn [offer]
                                                (:improvement/id (:improvement/_offers offer)))))
        lot-lines (for [lot (->> @state/island
                                 :island/lots)
                        :let [deed (:lot/deed lot)]
                        :when (= (:player/id (:player/_deeds deed))
                                 player-id)
                        :let [improvement (:lot/improvement lot)
                              revenue-offer (->> (improvement-id->offers (:improvement/id improvement))
                                                 (keep (fn [offer]
                                                         (offer-with-net-amount offer :effect.direction/to-player)))
                                                 first)
                              expense-offer (->> (improvement-id->offers (:improvement/id improvement))
                                                 (keep (fn [offer]
                                                         (offer-with-net-amount offer :effect.direction/from-player)))
                                                 first)]]
                    {:type ::lot
                     :id (:lot/id lot)
                     :lot lot
                     :improvement improvement
                     :revenue-offer revenue-offer
                     :expense-offer expense-offer
                     :deed-rate (- (:deed/rate deed))
                     :total (+ (:offer/net-amount revenue-offer)
                               (- (:offer/net-amount expense-offer))
                               (- (:deed/rate deed)))})
        debt-lines (->> @state/player
                        :player/loans
                        (map (fn [loan]
                               {:type ::loan
                                :id (:loan/id loan)
                                :total (- (:loan/daily-payment-amount loan))})))
        lines (concat lot-lines
                      debt-lines
                      [{:type ::demurrage
                        :id ::demurrage
                        :total (-> @state/player :player/private-stats :stats.private/stabilization-payment)}])]
    [:table
     [:tbody
      [:tr
       [:td {:tw "font-bold"} "Lot"]
       [:td]
       [:td {:tw "font-bold text-right px-4"} "Taxes"]
       [:td {:tw "font-bold text-right px-4"} "Expenses"]
       [:td {:tw "font-bold text-right px-4"} "Revenues"]
       [:td {:tw "font-bold text-right px-4"} "Income"]]
      (doall
        (for [{:keys [id type improvement lot revenue-offer expense-offer deed-rate total]} lines]
          ^{:key id}
          [:tr
           [:td
            (case type
              ::lot
              [:a {:href (pages/path-for [:page/lot {:island-id @state/island-id
                                                     :lot-id (:lot/id lot)}])}
               (:lot/x lot) "," (:lot/y lot)]
              ::loan
              [:a {:href (pages/path-for [:page/bank {:island-id @state/island-id}])}
               "loan"]
              ::demurrage
              [ui/label-with-info
               "eq"
               "to stabilize the economy and discourage cash hoarding, the government may charge a demurrage fee on cash balances, or provide interest"])]
           [:td
            [:a {:href (pages/path-for [:page/lot {:island-id @state/island-id
                                                   :lot-id (:lot/id lot)}])}
             (let [blueprint (blueprints/blueprints (:improvement/type improvement))]
               (:blueprint/icon blueprint))]]
           [:td {:tw "text-right tabular-nums px-4"}
            deed-rate]
           [:td {:tw "text-right tabular-nums px-4"}
            (when expense-offer
              [:div {:tw "flex items-center gap-1 justify-end"}
               [ui/resource-icon (blueprints/offer-exchange-resource (blueprints/offerables (:offer/type expense-offer)))]
               [ui/pie {:tw "w-1em h-1em"
                        :bg-color "#ddd"
                        :fg-color "green"}
                (:offer/utilization expense-offer)]
               [:span {:tw "grow"}
                (ui/format (:offer/net-amount expense-offer) 2)]])]
           [:td {:tw "text-right tabular-nums px-4"}
            (when revenue-offer
              [:div {:tw "flex items-center gap-1 justify-end"}
               [ui/resource-icon (blueprints/offer-exchange-resource (blueprints/offerables (:offer/type revenue-offer)))]
               [ui/pie {:tw "w-1em h-1em"
                        :bg-color "#ddd"
                        :fg-color "green"}
                (:offer/utilization revenue-offer)]
               [:span {:tw "grow"}
                (ui/format (:offer/net-amount revenue-offer) 2)]])]
           [:td {:tw ["text-right tabular-nums px-4"
                      (if (< total 0)
                        "text-red-600"
                        "text-blue-600")]}
            (ui/format total 2)]]))
      [:tr
       [:td {:tw "font-bold"} "Totals"]
       [:td]
       [:td {:tw "text-right tabular-nums px-4 font-bold"} (ui/format (reduce + (map :deed-rate lines)) 2)]
       [:td {:tw "text-right tabular-nums px-4 font-bold"} (ui/format (reduce + (map :offer/net-amount (map :revenue-offer lines))) 2)]
       [:td {:tw "text-right tabular-nums px-4 font-bold"} (ui/format (reduce + (map :offer/net-amount (map :expense-offer lines))) 2)]
       (let [total (reduce + (map :total lines))]
         [:td {:tw ["text-right tabular-nums px-4 font-bold"
                    (if (< total 0)
                      "text-red-600"
                      "text-blue-600")]}
          (ui/format total 2)])]]]))

(defn financial-report-view []
  [:section
   [:h1 "Financial Report"]
   (if (nil? @state/user)
     [ui/login-button]
     [:div
      [cashflow-graph]
      [cashflow-table]])])

(defn page []
  [map/page-wrapper
   [financial-report-view]])

(pages/register-page!
  {:page/id :page/finances
   :page/view #'page
   :page/path "/island/:island-id/finances"
   :page/parameters {:island-id :uuid}
   :page/on-enter! (fn [[_ {:keys [island-id]}]]
                     (state/set-island-id! island-id))})
