(ns georgetown.client.ui.pages.finances
  (:require
    [bloom.commons.pages :as pages]
    [com.rpl.specter :as x]
    [georgetown.client.ui.common :as ui]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.client.state :as state]
    [georgetown.client.ui.dataviz :as dataviz]
    [georgetown.client.ui.map :as map]
    [georgetown.client.ui.table :as table]
    [georgetown.sim.time :as time]))

(defn cashflow-graph []
  [:div {:tw "flex justify-end p-4"}
   [dataviz/plus-minus-sparkline
    ;; per-tick cashflows, summed into per-day bars
    (->> @state/private-stats-history
         (x/select [x/ALL :stats.private/net-cashflow])
         (partition time/ticks-per-day)
         (map (fn [day-values]
                (reduce + day-values))))]])

(defn per-day-total
  [per-tick-values]
  ;; extrapolate to a full day when fewer ticks are cached
  (if (seq per-tick-values)
    (* time/ticks-per-day
       (/ (reduce + per-tick-values)
          (count per-tick-values)))
    0))

(defn offer-with-net-amount
  [offer direction]
  (let [offerable (blueprints/offerables (:offer/type offer))
        per-unit (blueprints/effect-sum offer direction :resource/money)]
    (when (pos? per-unit)
      (let [tick-utilizations (->> @state/offer-utilization-history
                                   (map (fn [offer-id->utilization]
                                          (or (offer-id->utilization (:offer/id offer))
                                              0))))]
        (assoc offer
          :offer/tick-utilizations tick-utilizations
          :offer/net-amount (* (per-day-total tick-utilizations)
                               (or (:offerable/capacity offerable) 1)
                               per-unit))))))

(defn offer-cell
  [offer]
  (when offer
    [:div {:tw "flex items-center gap-1 justify-end"}
     (:offerable/icon (blueprints/offerables (:offer/type offer)))
     ;; one pie per tick, oldest first
     (doall
       (for [[tick-index utilization] (->> (:offer/tick-utilizations offer)
                                           reverse
                                           (map-indexed vector))]
         ^{:key tick-index}
         [ui/pie {:tw "w-1em h-1em"
                  :bg-color "#ddd"
                  :fg-color "green"}
          utilization]))
     [:span {:tw "grow"}
      (ui/format (:offer/net-amount offer) 2)]]))

(defn amount
  [value]
  [:span {:tw (if (< value 0)
                "text-red-600"
                "text-blue-600")}
   (ui/format value 2)])

(defn lot-link
  [lot content]
  [:a {:href (pages/path-for [:page/lot {:island-id @state/island-id
                                         :lot-id (:lot/id lot)}])}
   content])

(defn cashflow-lines []
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
                    ;; deed rate is charged each tick; show per-day
                    (let [deed-rate-per-day (- (* time/ticks-per-day (:deed/rate deed)))]
                      {:type ::lot
                       :id (:lot/id lot)
                       :label (str (:lot/x lot) "," (:lot/y lot))
                       :lot lot
                       :improvement improvement
                       :revenue-offer revenue-offer
                       :expense-offer expense-offer
                       :deed-rate deed-rate-per-day
                       :total (+ (:offer/net-amount revenue-offer)
                                 (- (:offer/net-amount expense-offer))
                                 deed-rate-per-day)}))
        debt-lines (->> @state/player
                        :player/loans
                        (map (fn [loan]
                               {:type ::loan
                                :id (:loan/id loan)
                                :label "loan"
                                ;; already per-day (charged once, on the night tick)
                                :total (- (:loan/daily-payment-amount loan))})))]
    (concat lot-lines
            debt-lines
            [{:type ::demurrage
              :id ::demurrage
              :label "eq"
              ;; stabilization payment is per-tick; show per-day
              :total (per-day-total
                       (->> @state/private-stats-history
                            (take time/ticks-per-day)
                            (map :stats.private/stabilization-payment)))}])))

(defn cashflow-table []
  [table/sortable-table
   {:table/caption "amounts per day"
    :table/rows (cashflow-lines)
    :table/row-key :id
    :table/columns
    [{:column/key :column/lot
      :column/label "Lot"
      :column/value :label
      :column/render (fn [{:keys [type lot label]}]
                       (case type
                         ::lot
                         [lot-link lot label]
                         ::loan
                         [:a {:href (pages/path-for [:page/bank {:island-id @state/island-id}])}
                          label]
                         ::demurrage
                         [ui/label-with-info
                          label
                          "to stabilize the economy and discourage cash hoarding, the government may charge a demurrage fee on cash balances, or provide interest"]))
      :column/footer (fn [_]
                       "Totals")}
     {:column/key :column/improvement
      :column/value (fn [line]
                      (str (:improvement/type (:improvement line))))
      :column/render (fn [{:keys [type lot improvement]}]
                       (when (= ::lot type)
                         [lot-link lot (:blueprint/icon (blueprints/blueprints (:improvement/type improvement)))]))}
     {:column/key :column/taxes
      :column/label "Taxes"
      :column/alignment :alignment/right
      :column/value :deed-rate
      :column/render (fn [line]
                       (ui/format (:deed-rate line) 2))
      :column/footer (fn [lines]
                       (ui/format (->> lines
                                       (map :deed-rate)
                                       (reduce +))
                                  2))}
     {:column/key :column/expenses
      :column/label "Expenses"
      :column/alignment :alignment/right
      :column/value (fn [line]
                      (:offer/net-amount (:expense-offer line)))
      :column/render (fn [line]
                       [offer-cell (:expense-offer line)])
      :column/footer (fn [lines]
                       (ui/format (->> lines
                                       (map :expense-offer)
                                       (map :offer/net-amount)
                                       (reduce +))
                                  2))}
     {:column/key :column/revenues
      :column/label "Revenues"
      :column/alignment :alignment/right
      :column/value (fn [line]
                      (:offer/net-amount (:revenue-offer line)))
      :column/render (fn [line]
                       [offer-cell (:revenue-offer line)])
      :column/footer (fn [lines]
                       (ui/format (->> lines
                                       (map :revenue-offer)
                                       (map :offer/net-amount)
                                       (reduce +))
                                  2))}
     {:column/key :column/income
      :column/label "Income"
      :column/alignment :alignment/right
      :column/value :total
      :column/render (fn [line]
                       [amount (:total line)])
      :column/footer (fn [lines]
                       [amount (->> lines
                                    (map :total)
                                    (reduce +))])}]}])

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
