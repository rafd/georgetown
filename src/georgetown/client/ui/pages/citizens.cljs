(ns georgetown.client.ui.pages.citizens
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.client.state :as state]
    [georgetown.client.ui.common :as ui]
    [georgetown.client.ui.map :as map]
    [georgetown.client.ui.table :as table]
    [georgetown.sim.time :as time]))

(def columns
  [{:column/key :column/citizen
    :column/label "Citizen"
    :column/class "text-sm font-bold"
    :column/value (fn [row]
                    (ui/citizen-display-name (:citizen row)))
    :column/render (fn [row]
                     [:<>
                      [ui/resource-icon :resource/citizen]
                      " "
                      (ui/citizen-display-name (:citizen row))])}
   {:column/key :column/savings
    :column/label "💰"
    :column/title "savings"
    :column/alignment :alignment/right
    :column/value (fn [row]
                    (:citizen/savings (:citizen row)))
    :column/render (fn [row]
                     (ui/format (:citizen/savings (:citizen row)) 0))}
   {:column/key :column/residency
    :column/label "🏝️"
    :column/title "years on island"
    :column/alignment :alignment/right
    :column/value (fn [row]
                    (:citizen/residency-ticks (:citizen row)))
    :column/render (fn [row]
                     (ui/format (time/ticks->years (:citizen/residency-ticks (:citizen row))) 1))}
   {:column/key :column/physical-stress
    :column/label "😰"
    :column/title "physical stress"
    :column/alignment :alignment/right
    :column/value (fn [row]
                    (:citizen/physical-stress (:citizen row)))
    :column/render (fn [row]
                     (ui/format (:citizen/physical-stress (:citizen row)) 2))}
   {:column/key :column/mental-stress
    :column/label "🤯"
    :column/title "mental stress"
    :column/alignment :alignment/right
    :column/value (fn [row]
                    (:citizen/mental-stress (:citizen row)))
    :column/render (fn [row]
                     (ui/format (:citizen/mental-stress (:citizen row)) 2))}
   {:column/key :column/hungry
    :column/label "Hungry?"
    :column/alignment :alignment/center
    :column/class "text-xs font-bold text-red-600"
    :column/value (fn [row]
                    (:citizen-state/hungry? (:citizen-state row)))
    :column/render (fn [row]
                     (when (:citizen-state/hungry? (:citizen-state row))
                       "HUNGRY"))}
   {:column/key :column/unhoused
    :column/label "Unhoused?"
    :column/alignment :alignment/center
    :column/class "text-xs font-bold text-red-600"
    :column/value (fn [row]
                    (:citizen-state/unhoused? (:citizen-state row)))
    :column/render (fn [row]
                     (when (:citizen-state/unhoused? (:citizen-state row))
                       "UNHOUSED"))}
   {:column/key :column/activity
    :column/label "Activity"
    :column/value (fn [row]
                    (str (:citizen-state/last-activity (:citizen-state row))))
    :column/render (fn [row]
                     [ui/last-activity-view (:citizen-state/last-activity (:citizen-state row))])}])

(defn citizens-view []
  (let [citizens (:island/citizens @state/island)
        citizen-states (:sim.out/citizen-states (:island/public-stats @state/island))]
    [:section
     [:h1 "Citizens"]
     [:p (count citizens) " citizens"]
     [table/sortable-table
      {:table/columns columns
       :table/rows (for [citizen citizens]
                     {:citizen citizen
                      :citizen-state (get citizen-states (:citizen/id citizen))})
       :table/row-key (fn [row]
                        (:citizen/id (:citizen row)))
       :table/row-attributes (fn [row]
                               {:tw "cursor-pointer hover:bg-blue-100"
                                :on-click (fn [_]
                                            (pages/navigate-to! [:page/citizen {:island-id @state/island-id
                                                                                :citizen-id (:citizen/id (:citizen row))}]))})}]]))

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
