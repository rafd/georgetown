(ns georgetown.client.ui.pages.citizens
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.client.state :as state]
    [georgetown.client.ui.common :as ui]
    [georgetown.client.ui.map :as map]))

(defn citizen-row
  [citizen citizen-state]
  [:tr {:tw "cursor-pointer hover:bg-blue-100"
        :on-click (fn [_]
                    (pages/navigate-to! [:page/citizen {:island-id @state/island-id
                                                        :citizen-id (:citizen/id citizen)}]))}
   [:td {:tw "text-sm font-bold pr-2"}
    [ui/resource-icon :resource/citizen] " " (subs (str (:citizen/id citizen)) 0 8)]
   [:td {:tw "text-right tabular-nums pr-2"}
    (ui/format (:citizen/savings citizen) 0)]
   [:td {:tw "text-right tabular-nums pr-2"}
    (ui/format (:citizen/physical-stress citizen) 2)]
   [:td {:tw "text-right tabular-nums pr-2"}
    (ui/format (:citizen/mental-stress citizen) 2)]
   [:td {:tw "text-center text-xs font-bold text-red-600 pr-2"}
    (when (:citizen-state/hungry? citizen-state)
      "HUNGRY")]
   [:td {:tw "text-center text-xs font-bold text-red-600 pr-2"}
    (when (:citizen-state/unhoused? citizen-state)
      "UNHOUSED")]
   [:td
    [ui/last-activity-view (:citizen-state/last-activity citizen-state)]]])

(defn citizens-view []
  (let [citizens (:island/citizens @state/island)
        citizen-states (:sim.out/citizen-states (:island/public-stats @state/island))]
    [:section
     [:h1 "Citizens"]
     [:p (count citizens) " citizens"]
     [:table
      [:thead
       [:tr {:tw "text-xs text-gray-500"}
        [:th {:tw "text-left pr-2"} "Citizen"]
        [:th {:tw "text-right pr-2"
              :title "savings"} "💰"]
        [:th {:tw "text-right pr-2"
              :title "physical stress"} "😰"]
        [:th {:tw "text-right pr-2"
              :title "mental stress"} "🤯"]
        [:th {:tw "pr-2"} "Hungry?"]
        [:th {:tw "pr-2"} "Unhoused?"]
        [:th {:tw "text-left"} "Activity"]]]
      [:tbody
       (doall
         (for [citizen citizens]
           ^{:key (:citizen/id citizen)}
           [citizen-row citizen (get citizen-states (:citizen/id citizen))]))]]]))

(defn page [_]
  [map/page-wrapper
   [citizens-view]])

(pages/register-page!
  {:page/id :page/citizens
   :page/view #'page
   :page/path "/island/:island-id/citizens"
   :page/parameters {:island-id :uuid}
   :page/on-enter! (fn [[_ {:keys [island-id]}]]
                     (state/set-island-id! island-id))})
