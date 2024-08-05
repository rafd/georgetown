(ns georgetown.ui.app
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.ui.map :as map]))

(defn app-view []
  [:div {:tw "flex"}
   [map/map-view]
   [:div {:tw "p-2"}
    [pages/current-page-view]]])
