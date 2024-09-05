(ns georgetown.ui.pages.finances
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.ui.common :as ui]
    [georgetown.schema :as schema]
    [georgetown.client.state :as state]
    [georgetown.ui.map :as map]))

(defn financial-report-view []
  [:section
   [:h1 "Financial Report"]
   (if (nil? @state/user)
     [ui/login-button]
     (let [resident-id (:resident/id @state/resident)
           improvement-id->offers (->> @state/offers
                                       (group-by (fn [offer]
                                                   (first (:offer/id offer)))))
           lines (for [lot (->> @state/island
                                :island/lots)
                       :let [deed (:lot/deed lot)]
                       :when (= (:resident/id (:resident/_deeds deed))
                                resident-id)
                       :let [improvement (:lot/improvement lot)
                             income-offer (->> (improvement-id->offers (:improvement/id improvement))
                                               (keep (fn [offer]
                                                       (let [offerable (schema/offerables (:offer/type offer))]
                                                         (when (= :resource/money (:offerable/demand-unit offerable))
                                                           (assoc offer :offer/net-amount
                                                             (* (:offer/utilization offer)
                                                                (or (:offerable/demand-amount offerable)
                                                                    (:offer/amount offer))))))))
                                               first)
                             expense-offer (->> (improvement-id->offers (:improvement/id improvement))
                                                (keep (fn [offer]
                                                        (let [offerable (schema/offerables (:offer/type offer))]
                                                          (when (= :resource/money (:offerable/supply-unit offerable))
                                                            (assoc offer
                                                              :offer/net-amount
                                                              (* (:offer/utilization offer)
                                                                 (or (:offerable/supply-amount offerable)
                                                                     (:offer/amount offer))))))))
                                                first)]]
                   {:lot lot
                    :improvement improvement
                    :income-offer income-offer
                    :expense-offer expense-offer
                    :deed-rate (- (:deed/rate deed))
                    :total (+ (:offer/net-amount income-offer)
                              (- (:offer/net-amount expense-offer))
                              (- (:deed/rate deed)))})]
       [:table
        [:tbody
         [:tr
          [:td {:tw "font-bold"} "Lot"]
          [:td]
          [:td {:tw "font-bold text-right px-4"} "Tax"]
          [:td {:tw "font-bold text-right px-4"} "Incomes"]
          [:td {:tw "font-bold text-right px-4"} "Expenses"]
          [:td {:tw "font-bold text-right px-4"} "Net"]]
         (doall
           (for [{:keys [improvement lot income-offer expense-offer deed-rate total]} lines]
             ^{:key (:lot/id lot)}
             [:tr
              [:td
               [:a {:href (pages/path-for [:page/lot {:island-id @state/island-id
                                                      :lot-id (:lot/id lot)}])}
                (:lot/x lot) "," (:lot/y lot)]]
              [:td
               [:a {:href (pages/path-for [:page/lot {:island-id @state/island-id
                                                      :lot-id (:lot/id lot)}])}
                (let [blueprint (schema/blueprints (:improvement/type improvement))]
                  (:blueprint/icon blueprint))]]
              [:td {:tw "text-right tabular-nums px-4"}
               deed-rate]
              [:<>
               [:td {:tw "text-right tabular-nums px-4"}
                (when income-offer
                  [:div {:tw "flex items-center gap-1 justify-end"}
                   [ui/resource-icon (:offerable/supply-unit (schema/offerables (:offer/type income-offer)))]
                   [ui/pie {:tw "w-1em h-1em"
                            :bg-color "#ddd"
                            :fg-color "green"}
                    (:offer/utilization income-offer)]
                   [:span {:tw "grow"}
                    (ui/format (:offer/net-amount income-offer) 2)]])]
               [:td {:tw "text-right tabular-nums px-4"}
                (when expense-offer
                  [:div {:tw "flex items-center gap-1 justify-end"}
                   [ui/resource-icon (:offerable/demand-unit (schema/offerables (:offer/type expense-offer)))]
                   [ui/pie {:tw "w-1em h-1em"
                            :bg-color "#ddd"
                            :fg-color "green"}
                    (:offer/utilization expense-offer)]
                   [:span {:tw "grow"}
                    (ui/format (:offer/net-amount expense-offer) 2)]])]]
              [:td {:tw ["text-right tabular-nums px-4"
                         (if (< total 0)
                           "text-red-600"
                           "text-blue-600")]}
               (ui/format total 2)]]))
         [:tr
          [:td {:tw "font-bold"} "Totals"]
          [:td]
          [:td {:tw "text-right tabular-nums px-4 font-bold"} (ui/format (reduce + (map :deed-rate lines)) 2)]
          [:td {:tw "text-right tabular-nums px-4 font-bold"} (ui/format (reduce + (map :offer/net-amount (map :income-offer lines))) 2)]
          [:td {:tw "text-right tabular-nums px-4 font-bold"} (ui/format (reduce + (map :offer/net-amount (map :expense-offer lines))) 2)]
          (let [total (reduce + (map :total lines))]
            [:td {:tw ["text-right tabular-nums px-4 font-bold"
                       (if (< total 0)
                         "text-red-600"
                         "text-blue-600")]}
             (ui/format total 2)])]]]))])

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
