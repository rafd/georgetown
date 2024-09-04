(ns georgetown.ui.map
  (:require
    [reagent.core :as r]
    [bloom.commons.pages :as pages]
    [georgetown.schema :as schema]
    [georgetown.client.state :as state]
    [georgetown.ui.common :as ui]))

(def tile-size 4)

(defn ->color [x]
  (str "oklch(50% 70% " (hash x) ")"))

(defn pie [opts percent]
  [:svg (assoc opts :height 20 :width 20 :view-box "0 0 20 20")
   [:circle {:cx 10
             :cy 10
             :r 10
             :fill "#333"}]
   [:circle {:cx 10
             :cy 10
             :r 5
             :fill "transparent"
             :stroke "green"
             :stroke-width 10
             :transform "rotate(-90) translate(-20)"
             :stroke-dasharray (str (* 31.4 percent) " 31.4")}]])

(defn version-view []
  (r/with-let [v (state/exec-atom! :query/version {})]
    [:a {:href "https://github.com/rafd/georgetown"}
     "v." (or (:version @v) "???")]))

(defn map-view []
  (when-let [island @state/island]
    (let [lots (:island/lots island)
          x-range (:lot/x (apply max-key :lot/x lots))
          y-range (:lot/y (apply max-key :lot/y lots))
          lot-count-width (inc x-range)
          lot-count-height (inc y-range)
          improvement-id->offers (->> @state/offers
                                      (group-by (fn [offer]
                                                  (first (:offer/id offer)))))]
      [:div.wrapper {:tw "relative w-60% h-screen bg-blue-500"}
       [:div.menu {:tw "absolute top-0 left-0 right-0 z-20 px-1 flex justify-between"}
        [:div {:tw "space-x-1"}
         [:a {:href (pages/path-for [:page/home {}])
              :title "home"}
          "🌎"]
         [:a {:href (pages/path-for [:page/island {:island-id (:island/id island)}])
              :title "island"}
          "🏝️"]
         [:a {:href (pages/path-for [:page/gazette {:island-id (:island/id island)}])
              :title "gazette"}
          "📈"]]
        (when @state/resident
          [:a {:tw "bg-white px-1"
               :href (pages/path-for [:page/finances {:island-id (:island/id island)}])
               :title "financial report"}
           [ui/resource-amount @state/money-balance :resource/money]])]

       [:div.scrollable-map {:tw "overflow-auto h-full w-full z-10"}
        [:div.lots {:tw "relative"
                    :style {:padding (str tile-size "em")
                            :box-sizing "content-box"
                            :width (str (* lot-count-width tile-size) "em")
                            :height (str (* lot-count-height tile-size) "em")}}
         [:div.title {:tw "absolute bottom-0 left-0 text-blue-800 px-1"}
          [version-view]]
         (doall
           (for [[y row-lots] (->> lots
                                   (sort-by (juxt :lot/x :lot/y))
                                   (group-by :lot/y)
                                   (sort-by key))]
             ^{:key y}
             [:div.row {:tw "flex"}
              (doall
                (for [lot row-lots
                      :let [deed (:lot/deed lot)
                            improvement (:lot/improvement lot)]]
                  ^{:key (:lot/x lot)}
                  [:a.lot
                   {:tw "group shrink-0"
                    :href (pages/path-for [:page/lot {:island-id (:island/id island)
                                                      :lot-id (:lot/id lot)}])
                    :style {:width (str tile-size "em")
                            :height (str tile-size "em")
                            :background (if (pages/active?
                                              [:page/lot {:island-id (:island/id island)
                                                          :lot-id (:lot/id lot)}])
                                          "#026002"
                                          "green")
                            :border-right "1px solid #009600"
                            :border-bottom "1px solid #009600"
                            :color "white"}}
                   (when deed
                     [:div.deed
                      {:tw "h-full block relative"
                       :style {:border-width "2px"
                               :border-style "solid"
                               :border-color (->color (:user/id (:user/_residents (:resident/_deeds deed))))}}
                      [:div.rate
                       {:tw "absolute top-0 left-0 py-0.5 px-1 text-white tabular-nums"
                        :style {:font-size "0.5em"
                                :background-color (->color (:user/id (:user/_residents (:resident/_deeds deed))))}}
                       (:deed/rate deed)]
                      (when improvement
                        [:div.improvement
                         [:div.icon {:style {:font-size (str (/ tile-size 2) "em")
                                             :text-align "center"
                                             :pointer-events "none"
                                             :line-height (str (/ tile-size 2) "em")}}
                          (let [blueprint (schema/blueprints (:improvement/type improvement))]
                            (:blueprint/icon blueprint))]
                         [:div.offers
                          {:tw "absolute bottom-0 right-0 flex gap-0.5"
                           :style {:font-size "0.3em"}}
                          (for [offer (->> improvement
                                           :improvement/id
                                           improvement-id->offers
                                           (sort-by (juxt :offer/invert? :offer/type))
                                           reverse)
                                :let [offerable (schema/offerables (:offer/type offer))]]
                            ^{:key (:offer/id offer)}
                            [:div.offer
                             {:tw "bg-black px-1 py-0.5 gap-0.5 flex items-center"
                              :style {:font-size "0.5em"}}
                             [ui/resource-icon (if (:offerable/invert? offerable)
                                                 (:offerable/demand-unit offerable)
                                                 (:offerable/supply-unit offerable))]
                             [:div {:title (Math/round (* 100 (:offer/utilization offer)))}
                              [pie {:tw "w-0.6rem h-0.6rem"} (:offer/utilization offer)]]])]])])]))]))]]])))

(defn page-wrapper
  [sidebar]
  [:div {:tw "flex w-screen h-screen"}
   [map-view]
   [:div {:tw "p-2 overflow-auto"}
    sidebar]])
