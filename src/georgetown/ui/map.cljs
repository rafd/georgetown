(ns georgetown.ui.map
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.schema :as schema]
    [georgetown.client.state :as state]))

(def tile-size 4)

(defn ->color [x]
  (str "oklch(50% 70% " (hash x) ")"))

(defn map-view []
  (when-let [lots @state/lots]
    (let [x-range (apply max-key :lot/x lots)
          y-range (apply max-key :lot/y lots)
          lot-count-width (inc x-range)
          lot-count-height (inc y-range)
          improvement-id->offers (->> @state/offers
                                      (group-by (fn [offer]
                                                  (first (:offer/id offer)))))]
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
                    :let [deed (first (:deed/_lot lot))
                          improvement (first (:improvement/_lot lot))]]
                ^{:key (:lot/x lot)}
                [:a.lot
                 {:href (pages/path-for [:page/lot {:id (:lot/id lot)}])
                  :style {:width (str tile-size "em")
                          :height (str tile-size "em")
                          :background (if (pages/active? [:page/lot {:id (:lot/id lot)}])
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
                             :border-color (->color (:user/id (:deed/owner deed)))}}
                    [:div.rate
                     {:tw "absolute top-0 left-0 py-0.5 px-1 text-white tabular-nums"
                      :style {:font-size "0.5em"
                              :background-color (->color (:user/id (:deed/owner deed)))}}
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
                        (for [offer (improvement-id->offers (:improvement/id improvement))]
                          ^{:key (:offer/id offer)}
                          [:div.offer
                           {:tw "bg-black text-white py-0.5 px-1 tabular-nums"
                            :style {:font-size "0.5em"}}
                           (:offer/amount offer)])]])])]))]))])))


