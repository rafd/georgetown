(ns georgetown.ui.map
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.schema :as schema]
    [georgetown.client.state :as state]
    [georgetown.ui.common :as ui]))

(def tile-size 4)

(defn ->color [x]
  (str "oklch(50% 70% " (hash x) ")"))

(defn resident-view []
  [:div {:tw "bg-white p-2"}
   [ui/resource-amount @state/money-balance :resource/money]])

(defn map-view []
  (when-let [island @state/island]
    (let [lots (:island/lots island)
          x-range (apply max-key :lot/x lots)
          y-range (apply max-key :lot/y lots)
          lot-count-width (inc x-range)
          lot-count-height (inc y-range)
          improvement-id->offers (->> @state/offers
                                      (group-by (fn [offer]
                                                  (first (:offer/id offer)))))]
      [:div {:tw "relative"}
       [:div.menu {:tw "absolute top-0 right-0"}
        [:a {:href (pages/path-for [:page/home {}])} "🌍"]
        [:a {:href (pages/path-for [:page/island {:island-id (:island/id island)}])}
         "🏠"]
        [:a {:href (pages/path-for [:page/gazette {:island-id (:island/id island)}])
             :title "gazette"}
         "📈"]]
       (when @state/resident
         [:div {:tw "absolute bottom-0 left-0"}
          [resident-view]])
       [:div {:style {:width (str (* lot-count-width tile-size) "em")
                      :height (str (* lot-count-height tile-size) "em")}}
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
                  {:tw "group"
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
                         {:tw "absolute bottom-0 right-0"
                          :style {:font-size "0.5em"}}
                         (for [offer (->> improvement
                                          :improvement/id
                                          improvement-id->offers
                                          (sort-by :offer/type))
                               :let [offerable (schema/offerables (:offer/type offer))]]
                           ^{:key (:offer/id offer)}
                           [:div.offer
                            {:tw "hidden group-hover:block bg-black text-white py-0.5 px-1 tabular-nums"
                             :style {:font-size "0.5em"}}
                            [ui/resource-amount
                             (or (:offerable/supply-amount offerable)
                                 (:offer/amount offer))
                             (:offerable/supply-unit offerable)]
                            [ui/resource-amount
                             (or (:offerable/demand-amount offerable)
                                 (:offer/amount offer))
                             (:offerable/demand-unit offerable)]])]])])]))]))]])))

(defn page-wrapper
  [sidebar]
  [:div {:tw "flex w-screen"}
   [map-view]
   [:div {:tw "p-2 overflow-x-auto"}
    sidebar]])
