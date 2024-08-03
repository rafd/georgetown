(ns georgetown.ui.app
  (:require
    [bloom.commons.pages :as pages]
    [reagent.core :as r]
    [georgetown.client.state :as state]))

(def tile-size 4)

(defn ->color [x]
  (str "oklch(50% 70% " (hash x) ")"))

(defn map-view []
  (r/with-let [lots (state/exec-atom! :query/all {})]
    (when @lots
      (let [x-range (apply max-key :lot/x @lots)
            y-range (apply max-key :lot/y @lots)
            lot-count-width (inc x-range)
            lot-count-height (inc y-range)]
        [:div {:style {:width (str (* lot-count-width tile-size) "em")
                       :height (str (* lot-count-height tile-size) "em")}}
         (for [[y row-lots] (->> @lots
                             (sort-by (juxt :lot/x :lot/y))
                             (group-by :lot/y)
                             (sort-by key))]
           ^{:key y}
           [:div {:tw "flex"}
            (for [lot row-lots
                  :let [deed (first (:deed/_lot lot))
                        improvement (first (:improvement/_lot lot))]]
              ^{:key (:lot/x lot)}
              [:div {:style {:width (str tile-size "em")
                             :height (str tile-size "em")
                             :background "green"
                             :border-right "1px solid gray"
                             :border-bottom "1px solid gray"
                             :color "white"}}
               (when deed
                 [:div {:tw "h-full"
                        :style {:border-width "2px"
                                :border-style "solid"
                                :border-color (->color (:user/id (:deed/owner deed)))}}
                  (when improvement
                    [:div {:style {:font-size (str (/ tile-size 2) "em")
                                   :text-align "center"
                                   :pointer-events "none"
                                   :line-height (str (/ tile-size 2) "em")}}
                     (case (:improvement/type improvement)
                       :improvement.type/house "🏠"
                       :improvement.type/farm "🌽"
                       nil)])])])])]))))

(defn app-view []
  [:div
   #_"welcome to georgetown"
   [map-view]
   #_[pages/current-page-view]])
