(ns georgetown.ui.pages.debug
  (:require
    [reagent.core :as r]
    [bloom.commons.pages :as pages]
    [georgetown.client.state :as state]
    [georgetown.biome :as biome]
    [georgetown.ui.iso :as iso]))

(defn grid [color-fn value-fn]
  (when-let [island @state/island]
    (let [lots (:island/lots island)
          tile-size 1]
      [:div {:style {:padding (str (* 2 tile-size) "rem")
                     :background-color (biome/color {:lot/x 0
                                                     :lot/y 1
                                                     :lot/elevation 0.0
                                                     :lot/moisture 0.0})}}
       (doall
         (for [[y row-lots] (->> lots
                                 (sort-by (juxt :lot/x :lot/y))
                                 (group-by :lot/y)
                                 (sort-by key))]
           ^{:key y}
           [:div.row {:tw "flex"}
            (doall
              (for [lot row-lots]
                ^{:key (:lot/x lot)}
                [:a.lot
                 {:tw "group shrink-0"
                  :style {:width (str tile-size "rem")
                          :height (str tile-size "rem")
                          :color "red"
                          :font-size "0.2em"
                          :text-align "center"
                          :line-height (str tile-size "rem")
                          :background-color (color-fn lot)}}
                 (value-fn lot)]))]))])))

(defn view [_]
  (r/with-let [islands (state/exec-atom! :query/islands {})]
    [:div {:tw "flex flex-wrap gap-4"}
     (when @islands
       (state/set-island-id! (:island/id (first @islands))))
     [:div
      "elevation"
      [grid
       (fn [lot]
         (str "oklch(" (:lot/elevation lot) " 0 0)"))
       (fn [lot]
         (Math/round (* 100 (:lot/elevation lot))))]]
     [:div
      "moisture"
      [grid
       (fn [lot]
         (str "oklch(" (:lot/moisture lot) " 0 0)"))
       (fn [lot]
         (Math/round (* 100 (:lot/moisture lot))))]]
     [:div
      "temperature"
      [grid
       (fn [lot]
         (str "oklch(" (/ (biome/elevation->temperature (:lot/elevation lot))
                          22) " 0 0)"))
       (fn [lot]
         (Math/floor (biome/elevation->temperature (:lot/elevation lot))))]]
     [:div
      "biome"
      [grid
       (fn [lot]
         (biome/->color (biome/biome (:lot/elevation lot) (:lot/moisture lot))))
       (fn [_]
         "")]]

     [:div
      "biome+elevation+grid"
      [grid
       (fn [lot]
         (biome/color lot))
       (fn [_]
         "")]]

     [:div
      "iso"
      [iso/iso-view]]]))

(pages/register-page!
  {:page/id :page/debug
   :page/view #'view
   :page/path "/debug"})
