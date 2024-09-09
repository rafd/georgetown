(ns georgetown.ui.pages.gazette
  (:require
    [bloom.commons.fontawesome :as fa]
    [bloom.commons.pages :as pages]
    [com.rpl.specter :as x]
    [georgetown.client.state :as state]
    [georgetown.schema :as schema]
    [georgetown.ui.dataviz :as dataviz]
    [georgetown.ui.map :as map]
    [georgetown.ui.common :as ui]))

(defn x-stats [path]
  (x/select path @state/public-stats-history))

(defn stats-view
  [island]
  [:section
   [:h1 "Census and Market Report"]
   (when-let [stats (:island/public-stats island)]
     [:table {:tw "text-sm"
              :style {:border-collapse "separate"
                      :border-spacing "0.5em"}}
      [:tbody
       [:tr
        [:td {:col-span 2} "money supply"]
        [:td {:tw "text-right"}
         [ui/resource-amount (:sim.out/net-money-balance stats) 0 :resource/money]]
        [:td
         [dataviz/multi-sparkline (x-stats [x/ALL :sim.out/net-money-balance])]]]
       [:tr
        [:td {:col-span 2} "citizen savings"]
        [:td {:tw "text-right"}
         [ui/resource-amount (:sim.out/citizen-money-balance stats) 0 :resource/money]
         "/"
         [ui/resource-amount (:sim.out/money-savings-goal stats) 0 :resource/money]]
        [:td
         [dataviz/multi-sparkline
          (x-stats [x/ALL :sim.out/citizen-money-balance])
          (x-stats [x/ALL :sim.out/money-savings-goal])]]]
       [:tr
        [:td {:tw "align-top"} "resident:citizen cash ratio"]
        [:td {:tw "align-top"}
         [:div "before"]
         [:div "after"]]
        [:td {:tw "text-right align-top"}
         [:div [ui/value (ui/format (:sim.out/cash-ratio-before stats) 3)]]
         [:div [ui/value (ui/format (:sim.out/cash-ratio-after stats) 3)]]]
        [:td
         [:div [dataviz/sparkline
                {:y-min 0 :y-max 1 :y-line 0.5 :bar-width 2}
                (x-stats [x/ALL :sim.out/cash-ratio-before])]]
         [:div [dataviz/sparkline
                {:y-min 0 :y-max 1 :y-line 0.5 :bar-width 2}
                (x-stats [x/ALL :sim.out/cash-ratio-after])]]]]
       [:tr
        [:td {:col-span 2}
         [ui/label-with-info
          "stabilization rate"
          "to stabilize the economy and discourage cash hoarding, the government may charge a demurrage fee on cash balances, or provide interest"]]
        [:td {:tw "text-right"} [ui/value (ui/format (:sim.out/stabilization-rate stats) 4)]]
        [:td [:div [dataviz/sparkline
                    {:y-min 0.8 :y-max 1.2 :y-line 1.0 :bar-width 2}
                    (x-stats [x/ALL :sim.out/stabilization-rate])]]]]
       [:tr
        [:td {:col-span 2} "citizen food reserves"]
        [:td {:tw "text-right"}
         [ui/resource-amount (:sim.out/citizen-food-balance stats) 0 :resource/food]
         "/"
         [ui/resource-amount (:sim.out/food-savings-goal stats) 0 :resource/food]]
        [:td
         [dataviz/multi-sparkline
          (x-stats [x/ALL :sim.out/citizen-food-balance])
          (x-stats [x/ALL :sim.out/food-savings-goal])]]]]
      [:tbody
       [:tr
        [:td {:tw "align-top"} "population"]
        [:td
         [:div "supportable"]
         [:div "current"]]
        [:td {:tw "text-right"}
         [:div [ui/resource-amount (:sim.out/max-supported-population stats) 0 :resource/citizen]]
         [:div [ui/resource-amount (:sim.out/population stats) 0 :resource/citizen]]]
        [:td
         [dataviz/multi-sparkline
          (x-stats [x/ALL :sim.out/population])
          (x-stats [x/ALL :sim.out/max-supported-population])]]
        [:td]
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
              [:td {:tw "align-top"}
               [:div "available"]
               [:div "demand"]
               [:div "supplied"]
               [:div "price"]
               [:div "cost"]]
              [:td {:tw "text-right tabular-nums align-top"}
               #_[:div [ui/resource-amount supply 0 resource-id]]
               [:div [ui/resource-amount available-supply 0 resource-id]]
               [:div [ui/resource-amount demand 0 resource-id]]
               [:div [ui/resource-amount supply 0 resource-id]]
               [:div
                (if invert?
                  [ui/resource-amount (/ 1 clearing-price) 2 resource-id resource-b-id]
                  [ui/resource-amount clearing-price 2 resource-b-id resource-id])]
               [:div [ui/resource-amount cost 0 resource-b-id]]]
              [:td
               [:span {:tw "text-xs"} "supply, demand"]
               [dataviz/multi-sparkline
                (x-stats [x/ALL :sim.out/resources resource-id :supply])
                (x-stats [x/ALL :sim.out/resources resource-id :available-supply])]
               [:span {:tw "text-xs"} "price"]
               [dataviz/multi-sparkline
                (x-stats [x/ALL :sim.out/resources resource-id :clearing-price])]
               [:span {:tw "text-xs"} "market"]
               [dataviz/market-graph-view demand tenders]]])))
       (let [{money-available-supply :available-supply money-demand :demand money-supply :supply} (get-in stats [:sim.out/resources :resource/money])
             {:keys [demand available-supply supply clearing-price]} (get-in stats [:sim.out/resources :resource/labour])]
         [:tr
          [:td {:tw "align-top"} "labour"]
          [:td
           [ui/label-with-info
            "available"
            "labour hours available in population"]
           [ui/label-with-info
            "offered"
            "labour hours offered by jobs"]
           [ui/label-with-info
            "transacted"
            "labour hours actually worked"]]
          [:td {:tw "text-right tabular-nums align-top"}
           [:div [ui/resource-amount available-supply 0 :resource/labour]]
           [:div [ui/resource-amount demand 0 :resource/labour]]
           [:div [ui/resource-amount supply 0 :resource/labour]]]
          [:td {:tw "align-top"}
           [:span {:tw "text-xs"} "transacted, available"]
           [dataviz/multi-sparkline
            (x-stats [x/ALL :sim.out/resources :resource/labour :supply])
            (x-stats [x/ALL :sim.out/resources :resource/labour :available-supply])]]
          [:td]
          [:td clearing-price]])
       [:tr
        [:td "leisure"]
        [:td "percent"]
        [:td {:tw "text-right tabular-nums align-top"}
         [ui/resource-amount (:sim.out/leisure-percent stats) 2 :resource/labour]]
        [:td
         [dataviz/multi-sparkline
          (x-stats [x/ALL :sim.out/leisure-percent])
          (map (constantly 1) (x-stats [x/ALL :sim.out/leisure-percent]))]]]]])])

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
