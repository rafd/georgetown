(ns georgetown.ui.pages.gazette
  (:require
    [bloom.commons.fontawesome :as fa]
    [bloom.commons.pages :as pages]
    [com.rpl.specter :as x]
    [georgetown.client.state :as state]
    [georgetown.schema :as schema]
    [georgetown.ui.map :as map]
    [georgetown.ui.common :as ui]))

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

(defn multi-sparkline
  [& datasets]
  (let [bar-width 2
        bar-height 25
        x-range (count (first datasets))
        right-pad 0
        width (+ (* x-range bar-width) right-pad)
        height bar-height
        colors ["#0000ff" "#9999ff"]
        y-range (apply max (flatten datasets))
        y-factor (/ height y-range)]
    [:div {:tw "flex gap-2px"}
     [:svg {:style {:width (str width "px")
                    :height (str height "px")}}
      (for [[dataset-index values] (map-indexed vector (reverse datasets))]
        ^{:key dataset-index}
        [:g
         ;; reverse values so the latest is on the right
         (for [[i value] (map-indexed vector (reverse values))
               ;; sometimes get nils (ex. no clearing price)
               :let [value (or value 0)]]
           ^{:key i}
           [:rect {:fill (get colors (dec (- (count datasets) dataset-index)))
                   :width bar-width
                   :height (max 0 (* y-factor value))
                   :x (* i bar-width)
                   :y (- height (* y-factor value))}])])
      ;; placing labels at position in graph
      ;; overlapping, so for now, disable
      #_[:g
         (for [[dataset-index values] (map-indexed vector datasets)
               :let [value (first values)]]
           ^{:key dataset-index}
           [:text {:fill "black"
                   :font-size "10px"
                   :font-variant "tabular-nums"
                   :alignment-baseline "hanging"
                   :x (+ 2 (* bar-width x-range))
                   :y (- bar-height (* y-factor value))}
            (ui/format value 0)])]]
     (let [sig-figs (if (every? (fn [x] (< 1 x)) (map first datasets))
                      0
                      2)]
       [:div {:tw "flex flex-col text-right tabular-nums -mt-2px justify-between"}
        (for [[dataset-index value] (map-indexed vector (sort > (map first datasets)))]
          ^{:key dataset-index}
          [:div {:style {:font-size "0.5rem" :line-height 1}}
           (ui/format value sig-figs)])])]))

(defn x-stats [path]
  (x/select path @state/stats-history))

(defn bar-graph-view [& values]
  [:div {:tw "w-150px bg-white"}
   [:div.numbers {:tw "flex justify-between"}
    (for [[i value] (map-indexed vector values)]
      ^{:key i}
      [:div {:tw "text-blue"
             :style {:font-size "0.7em"}}
       (ui/format value 0)])]
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
     [:table {:tw "text-sm"
              :style {:border-collapse "separate"
                      :border-spacing "0.5em"}}
      [:tbody
       [:tr
        [:td "money supply"]
        [:td]
        [:td {:tw "text-right"}
         [ui/resource-amount (:sim.out/net-money-balance stats) 0 :resource/money]]
        [:td
         [multi-sparkline (x-stats [x/ALL :sim.out/net-money-balance])]]]
       [:tr
        [:td "citizen savings"]
        [:td]
        [:td {:tw "text-right"}
         [ui/resource-amount (:sim.out/citizen-money-balance stats) 0 :resource/money]
         "/"
         [ui/resource-amount (:sim.out/money-savings-goal stats) 0 :resource/money]]
        [:td
         [multi-sparkline
          (x-stats [x/ALL :sim.out/citizen-money-balance])
          (x-stats [x/ALL :sim.out/money-savings-goal])]]]
       [:tr
        [:td "citizen food balance"]
        [:td]
        [:td {:tw "text-right"}
         [ui/resource-amount (:sim.out/citizen-food-balance stats) 0 :resource/food]
         "/"
         [ui/resource-amount (:sim.out/food-savings-goal stats) 0 :resource/food]]
        [:td
         [multi-sparkline
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
         [multi-sparkline
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
               [multi-sparkline
                (x-stats [x/ALL :sim.out/resources resource-id :supply])
                (x-stats [x/ALL :sim.out/resources resource-id :available-supply])]
               [:span {:tw "text-xs"} "price"]
               [multi-sparkline
                (x-stats [x/ALL :sim.out/resources resource-id :clearing-price])]
               [:span {:tw "text-xs"} "market"]
               [market-graph-view demand tenders]]])))
       (let [{money-available-supply :available-supply money-demand :demand money-supply :supply} (get-in stats [:sim.out/resources :resource/money])
             {:keys [demand available-supply supply clearing-price]} (get-in stats [:sim.out/resources :resource/labour])]
         [:tr
          [:td {:tw "align-top"} "labour"]
          [:td
           [:div {:tw "flex items-center gap-1"
                  :title "labour hours available in population"}
            "available"
            [fa/fa-info-circle-solid {:tw "w-0.75em w-0.75em text-gray-400"}]]
           [:div {:tw "flex items-center gap-1"
                  :title "labour hours offered by jobs"}
            "offered"
            [fa/fa-info-circle-solid {:tw "w-0.75em w-0.75em text-gray-400"}]]
           [:div {:tw "flex items-center gap-1"
                  :title "labour hours actually worked"}
            "transacted"
            [fa/fa-info-circle-solid {:tw "w-0.75em w-0.75em text-gray-400"}]]]
          [:td {:tw "text-right tabular-nums align-top"}
           [:div [ui/resource-amount available-supply 0 :resource/labour]]
           [:div [ui/resource-amount demand 0 :resource/labour]]
           [:div [ui/resource-amount supply 0 :resource/labour]]]
          [:td {:tw "align-top"}
           [:span {:tw "text-xs"} "transacted, available"]
           [multi-sparkline
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
         [multi-sparkline
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
