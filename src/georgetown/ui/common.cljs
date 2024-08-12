(ns georgetown.ui.common
  (:require
    [georgetown.schema :as schema]))

(defn resource-amount [amount & resource-ids]
  [:div {:tw "flex items-center gap-1"}
   [:span {:tw "tabular-nums"
           :style {:font-size "0.65em"}}
    (if amount
      (.toLocaleString amount)
      "?")]
   [:span
    (interpose [:span {:style {:font-size "0.6em"}} " / "]
               (for [resource-id resource-ids
                     :let [resource (schema/resources resource-id)]]
                 ^{:key resource-id}
                 [:span {:title (name resource-id)
                         :style {:font-size "0.65em"}}
                  (:resource/icon resource)]))]])
