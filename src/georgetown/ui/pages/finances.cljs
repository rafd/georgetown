(ns georgetown.ui.pages.finances
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.ui.common :as ui]
    [georgetown.schema :as schema]
    [georgetown.client.state :as state]
    [georgetown.ui.map :as map]))

(defn format [x]
  (when x
    (.toLocaleString x
                     "en-US" #js {:minimumFractionDigits 2
                                  :maximumFractionDigits 2})))

(defn financial-report-view []
  [:section
   [:h1 "Financial Report"]
   (if (nil? @state/user)
     [ui/login-button]
     (let [improvement-id->offers (->> @state/offers
                                       (group-by (fn [offer]
                                                   (first (:offer/id offer)))))
           lines (for [lot (->> @state/island
                                :island/lots)
                       :let [deed (:lot/deed lot)]
                       :when (= (:resident/id (:resident/_deeds deed))
                                (:resident/id @state/resident))
                       :let [improvement (:lot/improvement lot)
                             ;; TODO this is wrong
                             [income expense] (->> (improvement-id->offers (:improvement/id improvement))
                                                   (map (fn [offer]
                                                          (let [offerable (schema/offerables (:offer/type offer))]
                                                            (cond
                                                              (= :resource/money (:offerable/demand-unit offerable))
                                                              (* (:offer/utilization offer)
                                                                 (or (:offerable/demand-amount offerable)
                                                                     (:offer/amount offer)))
                                                              (= :resource/money (:offerable/supply-unit offerable))
                                                              (- (* (:offer/utilization offer)
                                                                    (or (:offerable/supply-amount offerable)
                                                                        (:offer/amount offer))))))))
                                                   (remove nil?)
                                                   reverse)]]
                   {:lot lot
                    :improvement improvement
                    :income income
                    :expense expense
                    :deed-rate (- (:deed/rate deed))
                    :total (+ income
                              expense
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
           (for [{:keys [improvement lot income expense deed-rate total]} lines]
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
               [:td {:tw "text-right tabular-nums px-4"} (format income)]
               [:td {:tw "text-right tabular-nums px-4"} (format expense)]]
              [:td {:tw ["text-right tabular-nums px-4"
                         (if (< total 0)
                           "text-red-600"
                           "text-blue-600")]}
               (format total)]]))
         [:tr
          [:td {:tw "font-bold"} "Totals"]
          [:td]
          [:td {:tw "text-right tabular-nums px-4 font-bold"} (format (reduce + (map :deed-rate lines)))]
          [:td {:tw "text-right tabular-nums px-4 font-bold"} (format (reduce + (map :income lines)))]
          [:td {:tw "text-right tabular-nums px-4 font-bold"} (format (reduce + (map :expense lines)))]
          (let [total (reduce + (map :total lines))]
            [:td {:tw ["text-right tabular-nums px-4 font-bold"
                       (if (< total 0)
                         "text-red-600"
                         "text-blue-600")]}
             (format total)])]]]))]
  )

(defn page []
  [map/page-wrapper
   [financial-report-view]
   ])

(pages/register-page!
  {:page/id :page/finances
   :page/view #'page
   :page/path "/island/:island-id/finances"
   :page/parameters {:island-id :uuid}
   :page/on-enter! (fn [[_ {:keys [island-id]}]]
                     (state/set-island-id! island-id))})
