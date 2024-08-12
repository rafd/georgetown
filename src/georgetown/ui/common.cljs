(ns georgetown.ui.common
  (:require
    [georgetown.schema :as schema]))

(defn resource-icon [resource-id]
  (let [resource (schema/resources resource-id)]
    [:span {:title (name resource-id)}
     (:resource/icon resource)]))

(defn resource-amount [amount & resource-ids]
  [:div {:tw "flex items-center gap-1"}
   [:span {:tw "tabular-nums"
           :style {:font-size "0.65em"}}
    (if amount
      (.toLocaleString amount)
      "?")]
   (into [:<>]
         (interpose [:span {:style {:font-size "0.6em"}} " / "]
                    (for [resource-id resource-ids]
                      ^{:key resource-id}
                      [:span {:style {:font-size "0.65em"}}
                       [resource-icon resource-id]])))])
