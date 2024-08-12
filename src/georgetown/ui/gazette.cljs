(ns georgetown.ui.gazette
  (:require
    [com.rpl.specter :as x]
    [georgetown.client.state :as state]
    [georgetown.schema :as schema]
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
  [demand tenders succesful-tenders]
  (let [width 100
        height 20
        succesful-tenders (set succesful-tenders)
        tenders (->> tenders
                     (map (fn [tender]
                            (-> tender
                                (assoc :tender/price
                                  (/ (get-in tender [:tender/demand 1])
                                     (get-in tender [:tender/supply 1])))
                                (assoc :tender/success?
                                  (contains? succesful-tenders tender)))))
                     (sort-by :tender/price))
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
           (for [tender tenders]
             [:div {:tw ["shrink-0 grow-0"
                         (if (:tender/success? tender)
                           "bg-green-300 odd:bg-green-400"
                           "bg-gray-300 odd:bg-gray-400")]
                    :style {:width (str (* x-factor (get-in tender [:tender/supply 1])) "px")
                            :height (str (* y-factor (:tender/price tender)) "px")}}]))
     [:div {:tw "absolute grow-0 shrink-0"
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
        [:td "pop"]
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
                 {:keys [demand available-supply supply price tenders succesful-tenders]}
                 (get-in stats [:sim.out/resources resource-id])]
             ^{:key resource-id}
             [:tr
              [:td (:resource/label resource)]
              [:td
               [bar-graph-view
                supply
                demand
                available-supply]]
              [:td [stats-sparkline [x/ALL :sim.out/resources resource-id :price]]]
              [:td
               (if invert?
                 [resource-amount (/ 1 price) resource-id resource-b-id]
                 [resource-amount price resource-b-id resource-id])]
              [:td [market-graph-view demand tenders succesful-tenders]]])))
       (let [{:keys [demand available-supply supply price]} (get-in stats [:sim.out/resources :resource/labour])]
         [:tr
          [:td "labour"]
          [:td
           [bar-graph-view
            supply
            demand
            available-supply]]
          [:td]
          [:td price]])
       [:tr
        [:td "joy"]
        [:td [bar-graph-view
              (:sim.out/joy stats)
              1]]
        [:td [stats-sparkline [x/ALL :sim.out/joy]]]]]])])
