(ns georgetown.ui.pages.gazette
  (:require
    [bloom.commons.pages :as pages]
    [com.rpl.specter :as x]
    [georgetown.client.state :as state]
    [georgetown.schema :as schema]
    [georgetown.ui.map :as map]
    [georgetown.ui.common :refer [resource-amount]]))

(defn sparkline
  [values]
  (let [width 60
        height 20
        y-range (apply max values)
        y-factor (/ height y-range)]
    [:svg {:style {:width (str width "px")
                   :height (str height "px")}}
     (for [[i value] (map-indexed vector values)]
       ^{:key i}
       [:rect {:fill (if (odd? i) "#9999ff" "#0000ff")
               :width 1
               :height (* y-factor value)
               :x i
               :y (- height (* y-factor value))}])]))

(defn stats-sparkline
  [path]
  [sparkline (x/select path @state/stats-history)])

(defn bar-graph-view [& values]
  [:div {:tw "w-150px bg-white"}
   [:div.numbers {:tw "flex justify-between"}
    (for [[i value] (map-indexed vector values)]
      ^{:key i}
      [:div {:tw "text-blue"
             :style {:font-size "0.7em"}}
       (.toLocaleString value)])]
   [:div.bars {:tw "relative h-1.5em"}
    (for [[i value] (map-indexed vector values)]
      ^{:key i}
      [:div {:style {:width (str (* 100 (/ value (last values))) "%")}
             :tw "absolute top-0 h-1em left-0 bottom-0 bg-#0000ff55"}])]])

(defn market-graph-view
  [demand tenders]
  (let [width 100
        height 20
        tenders (->> tenders
                     (map (fn [tender]
                            (-> tender
                                (assoc :tender/price
                                  (/ (get-in tender [:tender/demand 1])
                                     (get-in tender [:tender/supply 1]))))))
                     (sort-by :tender/price)
                     (sort-by (complement :tender/active?)))
        x-range (->> tenders
                     (map (fn [tender] (get-in tender [:tender/supply 1])))
                     (apply +))
        y-range (apply max (map :tender/price tenders))
        x-factor (/ width x-range)
        y-factor (/ height y-range)]
    [:div {:tw "flex relative items-end"
           :style {:width (str width "px")
                   :height (str height "px")}}
     (into [:<>]
           (for [tender tenders
                 :let [height (* y-factor (:tender/price tender))
                       width (* x-factor (get-in tender [:tender/supply 1]))]]
             [:div {:tw "shrink-0 grow-0 bg-gray-300 odd:bg-gray-400"
                    :style {:width (str width "px")
                            :height (str height "px")}}
              [:div {:tw "bg-#00ff0077"
                     :style {:margin-top (str (* height (- 1 (:tender/fill-ratio tender))) "px")
                             :height (str (* height (:tender/fill-ratio tender)) "px")
                             :width (str width "px")}}]]))
     [:div.line
      {:tw "absolute grow-0 shrink-0"
       :style {:width (str (* x-factor demand) "px")
               :left 0
               :height height
               :border-right "1px solid red"}}]]))

(defn stats-view
  [island]
  [:div
   (when-let [stats (:island/simulator-stats island)]
     [:table
      [:tbody
       [:tr
        [:td "government $ balance"]
        [:td]
        [:td {:tw "text-right"}
         [resource-amount (:sim.out/government-money-balance stats) :resource/money]]
        [:td [stats-sparkline [x/ALL :sim.out/government-money-balance]]]]
       [:tr
        [:td "citizen $ balance"]
        [:td]
        [:td {:tw "text-right"}
         [resource-amount (:sim.out/citizen-money-balance stats) :resource/money]
         "/"
         [resource-amount (:sim.out/money-savings-goal stats) :resource/money]]
        [:td [stats-sparkline [x/ALL :sim.out/citizen-money-balance]]]]
       [:tr
        [:td "citizen food balance"]
        [:td]
        [:td {:tw "text-right"}
         [resource-amount (:sim.out/citizen-food-balance stats) :resource/food]
         "/"
         [resource-amount (:sim.out/food-savings-goal stats) :resource/food]]
        [:td [stats-sparkline [x/ALL :sim.out/citizen-food-balance]]]]]
      [:tbody
       [:tr
        [:td {:tw "align-top"} "population"]
        [:td
         [:div "current"]
         [:div "max supportable"]]
        [:td {:tw "text-right"}
         [:div [resource-amount (:sim.out/population stats) :resource/citizen]]
         [:div [resource-amount (:sim.out/max-supported-population stats) :resource/citizen]]]
        [:td [bar-graph-view
              (:sim.out/population stats)
              (:sim.out/max-supported-population stats)]]
        [:td [stats-sparkline [x/ALL :sim.out/population]]]
        [:td]
        [:td]]
       (doall
         (for [[resource-id resource-b-id invert?]
               [[:resource/food :resource/money]
                [:resource/shelter :resource/money]
                [:resource/money :resource/labour true]]]
           (let [resource (schema/resources resource-id)
                 {:keys [demand available-supply supply clearing-price cost tenders]}
                 (get-in stats [:sim.out/resources resource-id])]
             ^{:key resource-id}
             [:tr
              [:td {:tw "align-top"} (:resource/label resource)]
              [:td
               [:div "demand"]
               [:div "available"]
               [:div "supplied"]
               [:div "cost"]]
              [:td {:tw "text-right tabular-nums align-top"}
               [:div [resource-amount demand resource-id]]
               #_[:div [resource-amount supply resource-id]]
               [:div [resource-amount available-supply resource-id]]
               [:div [resource-amount supply resource-id]]
               [:div [resource-amount cost resource-b-id]]]
              [:td
               [bar-graph-view
                supply
                available-supply]]
              [:td [stats-sparkline [x/ALL :sim.out/resources resource-id :clearing-price]]]
              [:td
               (if invert?
                 [resource-amount (/ 1 clearing-price) resource-id resource-b-id]
                 [resource-amount clearing-price resource-b-id resource-id])]
              [:td [market-graph-view demand tenders]]])))
       (let [{money-available-supply :available-supply money-demand :demand money-supply :supply} (get-in stats [:sim.out/resources :resource/money])
             {:keys [demand available-supply supply clearing-price]} (get-in stats [:sim.out/resources :resource/labour])]
         [:tr
          [:td {:tw "align-top"} "labour"]
          [:td
           [:div "transacted"]
           [:div "available"]
           [:div "on-offer"]
           ]
          [:td {:tw "text-right tabular-nums align-top"}
           [:div [resource-amount supply :resource/labour]]
           [:div [resource-amount available-supply :resource/labour]]
           [:div [resource-amount demand :resource/labour]]
           ]
          [:td
           [bar-graph-view
            supply
            demand
            available-supply]]
          [:td]
          [:td clearing-price]])
       [:tr
        [:td "leisure"]
        [:td]
        [:td]
        [:td [bar-graph-view
              (:sim.out/leisure-percent stats)
              1]]
        [:td [stats-sparkline [x/ALL :sim.out/leisure-percent]]]]]])])

(defn page
  [_]
  [map/page-wrapper
   [stats-view @state/island]])

(pages/register-page!
  {:page/id :page/gazette
   :page/view #'page
   :page/path "/island/:island-id/gazette"
   :page/parameters {:island-id :uuid}
   :page/on-enter! (fn [[_ {:keys [island-id]}]]
                     (state/set-island-id! island-id))})
