(ns georgetown.ui.pages.gazette
  (:require
    [bloom.commons.pages :as pages]
    [com.rpl.specter :as x]
    [georgetown.client.state :as state]
    [georgetown.schema :as schema]
    [georgetown.ui.dataviz :as dataviz]
    [georgetown.ui.map :as map]
    [georgetown.ui.common :as ui]))

(defn x-stats [path]
  (x/select path @state/public-stats-history))

(def kv-row-tw "flex justify-between items-center gap-2")

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
        [:td "money supply"]
        [:td {:tw "text-right"}
         [ui/resource-amount (:sim.out/net-money-balance stats) 0 :resource/money]]
        [:td
         [dataviz/multi-sparkline (x-stats [x/ALL :sim.out/net-money-balance])]]]
       [:tr
        [:td "sim savings"]
        [:td {:tw "text-right"}
         [ui/resource-amount (:sim.out/total-sim-savings stats) 0 :resource/money]]
        [:td
         [dataviz/multi-sparkline (x-stats [x/ALL :sim.out/total-sim-savings])]]]
       [:tr
        [:td {:tw "align-top"} "resident:citizen cash ratio"]
        [:td {:tw "text-right align-top"}
         [:div {:tw kv-row-tw}
          [:span "before"]
          [ui/value (ui/format (:sim.out/cash-ratio-before stats) 3)]]
         [:div {:tw kv-row-tw}
          [:span "after"]
          [ui/value (ui/format (:sim.out/cash-ratio-after stats) 3)]]]
        [:td
         [:div [dataviz/sparkline
                {:y-min 0 :y-max 1 :y-line 0.5 :bar-width 2}
                (x-stats [x/ALL :sim.out/cash-ratio-before])]]
         [:div [dataviz/sparkline
                {:y-min 0 :y-max 1 :y-line 0.5 :bar-width 2}
                (x-stats [x/ALL :sim.out/cash-ratio-after])]]]]
       [:tr
        [:td
         [ui/label-with-info
          "stabilization rate"
          "to stabilize the economy and discourage cash hoarding, the government may charge a demurrage fee on cash balances, or provide interest"]]
        [:td {:tw "text-right"} [ui/value (ui/format (:sim.out/stabilization-rate stats) 4)]]
        [:td [:div [dataviz/sparkline
                    {:y-min 0.8 :y-max 1.2 :y-line 1.0 :bar-width 2}
                    (x-stats [x/ALL :sim.out/stabilization-rate])]]]]]
      [:tbody
       [:tr
        [:td "population"]
        [:td {:tw "text-right"}
         [ui/resource-amount (:sim.out/population stats) 0 :resource/citizen]]
        [:td
         [dataviz/multi-sparkline (x-stats [x/ALL :sim.out/population])]]]
       [:tr
        [:td {:tw "align-top"} "in need"]
        [:td {:tw "text-right align-top"}
         [:div {:tw kv-row-tw}
          [:span "hungry"]
          [ui/resource-amount (:sim.out/hungry-count stats) 0 :resource/citizen]]
         [:div {:tw kv-row-tw}
          [:span "unhoused"]
          [ui/resource-amount (:sim.out/unhoused-count stats) 0 :resource/citizen]]]
        [:td
         [dataviz/multi-sparkline
          (x-stats [x/ALL :sim.out/hungry-count])
          (x-stats [x/ALL :sim.out/unhoused-count])]]]
       [:tr
        [:td {:tw "align-top"} "stress"]
        [:td {:tw "text-right align-top"}
         [:div {:tw kv-row-tw}
          [:span "physical"]
          [ui/value (ui/format (:sim.out/mean-physical-stress stats) 2)]]
         [:div {:tw kv-row-tw}
          [:span "mental"]
          [ui/value (ui/format (:sim.out/mean-mental-stress stats) 2)]]]
        [:td
         [:div [dataviz/sparkline
                {:y-min 0 :y-max 1 :y-line 0.5 :bar-width 2}
                (x-stats [x/ALL :sim.out/mean-physical-stress])]]
         [:div [dataviz/sparkline
                {:y-min 0 :y-max 1 :y-line 0.5 :bar-width 2}
                (x-stats [x/ALL :sim.out/mean-mental-stress])]]]]
       [:tr
        [:td "joy"]
        [:td {:tw "text-right"}
         [ui/resource-amount (:sim.out/joy stats) 0 :resource/joy]]
        [:td
         [dataviz/multi-sparkline (x-stats [x/ALL :sim.out/joy])]]]
       [:tr
        [:td {:tw "align-top"} "work"]
        [:td {:tw "text-right align-top"}
         [:div {:tw kv-row-tw}
          [:span "employed"]
          [ui/resource-amount (:sim.out/employed-count stats) 0 :resource/citizen]]
         [:div {:tw kv-row-tw}
          [:span "idle"]
          [ui/resource-amount (:sim.out/idle-count stats) 0 :resource/citizen]]]
        [:td
         [dataviz/multi-sparkline
          (x-stats [x/ALL :sim.out/employed-count])
          (x-stats [x/ALL :sim.out/idle-count])]]]
       (doall
         (for [resource-id [:resource/food :resource/shelter]]
           (let [resource (schema/resources resource-id)
                 {:keys [demand available-supply supply clearing-price cost]}
                 (get-in stats [:sim.out/resources resource-id])]
             ^{:key resource-id}
             [:tr
              [:td {:tw "align-top"} (:resource/label resource)]
              [:td {:tw "text-right tabular-nums align-top"}
               [:div {:tw kv-row-tw}
                [:span "available"]
                [ui/resource-amount available-supply 0 resource-id]]
               [:div {:tw kv-row-tw}
                [:span "demand"]
                [ui/resource-amount demand 0 resource-id]]
               [:div {:tw kv-row-tw}
                [:span "supplied"]
                [ui/resource-amount supply 0 resource-id]]
               [:div {:tw kv-row-tw}
                [:span "price"]
                [ui/resource-amount clearing-price 2 :resource/money resource-id]]
               [:div {:tw kv-row-tw}
                [:span "cost"]
                [ui/resource-amount cost 0 :resource/money]]]
              [:td
               [:span {:tw "text-xs"} "supply, demand"]
               [dataviz/multi-sparkline
                (x-stats [x/ALL :sim.out/resources resource-id :supply])
                (x-stats [x/ALL :sim.out/resources resource-id :available-supply])]
               [:span {:tw "text-xs"} "price"]
               [dataviz/multi-sparkline
                (x-stats [x/ALL :sim.out/resources resource-id :clearing-price])]]])))]])])

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
