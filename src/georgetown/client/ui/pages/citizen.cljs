(ns georgetown.client.ui.pages.citizen
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.client.state :as state]
    [georgetown.client.ui.common :as ui]
    [georgetown.client.ui.map :as map]
    [georgetown.sim.blueprints :as blueprints]
    [georgetown.sim.constants :as constants]
    [georgetown.sim.time :as time]))

;; fixed at generation, don't change over a citizen's lifetime
(def constant-stats
  [[:citizen/preference.security "Pref: Security" 0 1 2]
   [:citizen/preference.self-improvement "Pref: Self-Improvement" 0 1 2]
   [:citizen/preference.physical-stress "Pref: Physical Stress" 0 1 2]
   [:citizen/preference.mental-stress "Pref: Mental Stress" 0 1 2]
   [:citizen/preference.spiritual-activity "Pref: Spiritual Activity" 0 1 2]
   [:citizen/preference.social-activity "Pref: Social Activity" 0 1 2]
   [:citizen/preference.physical-activity "Pref: Physical Activity" 0 1 2]
   [:citizen/preference.intellectual-activity "Pref: Intellectual Activity" 0 1 2]
   [:citizen/talent.intellect "Talent: Intellect" 0 1 2]
   [:citizen/talent.fitness "Talent: Fitness" 0 1 2]
   [:citizen/talent.social "Talent: Social" 0 1 2]])

;; evolve over the course of the simulation
(def variable-stats
  ;; [stat-key label minimum maximum sig-figs value-fn?]
  [[:citizen/age-ticks "Age" 0 100 0 time/ticks->years]
   [:citizen/residency-ticks "Residency" 0 100 1 time/ticks->years]
   [:citizen/savings "Savings" 0 10000 0]
   [:citizen/skill.intellect "Skill: Intellect" 0 1 2]
   [:citizen/skill.fitness "Skill: Fitness" 0 1 2]
   [:citizen/skill.social "Skill: Social" 0 1 2]
   [:citizen/physical-stress "Physical Stress" 0 1 2]
   [:citizen/mental-stress "Mental Stress" 0 1 2]])

(defn stat-row
  [label value minimum maximum sig-figs]
  [:tr
   [:td {:tw "text-sm pr-2"} label]
   [:td {:tw "text-right tabular-nums pr-2"} (ui/format value sig-figs)]
   [:td
    [:div {:tw "relative w-6rem h-0.5rem bg-gray-200"}
     [:div {:tw "absolute top-0 bottom-0 w-2px bg-blue-600"
            :style {:left (str (* 100 (/ (- value minimum) (- maximum minimum))) "%")}}]]]])

(defn stat-table
  [citizen stats]
  [:table {:tw "border rounded"
           :style {:border-collapse "separate"
                   :border-spacing "0.5em"}}
   [:tbody
    (doall
      (for [[stat-key label minimum maximum sig-figs value-fn] stats
            :let [value (get citizen stat-key)]]
        ^{:key stat-key}
        [stat-row label
         (if value-fn
           (value-fn value)
           value)
         minimum maximum sig-figs]))]])

(defn activity-cell
  [activity]
  (cond
    (nil? activity)
    nil
    (= :activity/idle activity)
    [:span {:tw "text-gray-400"
            :title "idle"}
     "·"]
    :else
    (when-let [offerable (blueprints/offerables activity)]
      (let [blueprint (blueprints/offerable-id->blueprint activity)]
        [:span {:title (str (:blueprint/label blueprint) ": " (:offerable/label offerable))}
         (:blueprint/icon blueprint)
         (:offerable/icon offerable)]))))

(defn day-state-count-cell
  [day-states state-key]
  (let [ticks (->> day-states
                   (filter (fn [[_epoch tick-state]]
                             (get tick-state state-key)))
                   count)]
    [:td {:tw "text-center text-xs font-bold text-red-600"}
     (when (pos? ticks)
       (str ticks "×"))]))

(defn activity-history-view
  [citizen-id]
  (let [tick-states (->> @state/public-stats-history
                         ;; newest first in the cache; oldest first for display math
                         reverse
                         (keep (fn [stats]
                                 (when-let [tick-state (get-in stats [:sim.out/citizen-states citizen-id])]
                                   [(:sim.out/epoch stats) tick-state]))))
        by-day (->> tick-states
                    (group-by (fn [[epoch _]]
                                (quot epoch time/ticks-per-day))))]
    [:div
     [:h4 {:tw "text-xs font-bold"} "Activity History"]
     (if (empty? tick-states)
       [:p {:tw "text-sm text-gray-400"} "No activity observed yet."]
       [:div {:tw "max-h-20rem overflow-y-auto"}
        [:table {:tw "border rounded"
                 :style {:border-collapse "separate"
                         :border-spacing "0.5em"}}
         [:thead
          [:tr {:tw "text-xs text-gray-500"}
           [:th {:tw "text-right"} "Day"]
           (for [shift constants/shift-order]
             ^{:key shift}
             [:th (name shift)])
           [:th {:tw "text-right"} "savings"]
           [:th "hungry"]
           [:th "unhoused"]]]
         [:tbody
          (doall
            (for [day (sort (fn [day-a day-b]
                              (compare day-b day-a))
                            (keys by-day))
                  :let [day-states (by-day day)
                        shift->tick-state (->> day-states
                                               (map (fn [[epoch tick-state]]
                                                      [(constants/epoch->shift epoch) tick-state]))
                                               (into {}))]]
              ^{:key day}
              [:tr
               [:td {:tw "text-sm text-right tabular-nums"} day]
               (for [shift constants/shift-order]
                 ^{:key shift}
                 [:td {:tw "text-center"}
                  [activity-cell (:citizen-state/last-activity (shift->tick-state shift))]])
               [:td {:tw "text-right tabular-nums"}
                ;; day-states are in epoch order, so last = end of day
                (ui/format (:citizen-state/savings (second (last day-states))) 0)]
               [day-state-count-cell day-states :citizen-state/hungry?]
               [day-state-count-cell day-states :citizen-state/unhoused?]]))]]])]))

(defn citizen-view
  [citizen-id]
  (let [citizen (->> (:island/citizens @state/island)
                     (filter (fn [candidate]
                               (= citizen-id (:citizen/id candidate))))
                     first)]
    [:section
     [:a {:tw "text-sm text-blue-600"
          :href (pages/path-for [:page/citizens {:island-id @state/island-id}])}
      "← all citizens"]
     (if (nil? citizen)
       [:p "This citizen is no longer on the island."]
       [:div {:tw "space-y-2"}
        [:h1 {:tw "text-sm font-bold"}
         [ui/resource-icon :resource/citizen] " " (ui/citizen-display-name citizen)]
        [activity-history-view citizen-id]
        [:div {:tw "flex gap-2"}
         [:div
          [:h4 {:tw "text-xs font-bold"} "Constants"]
          [stat-table citizen constant-stats]]
         [:div
          [:h4 {:tw "text-xs font-bold"} "Variables"]
          [stat-table citizen variable-stats]]]])]))

(defn page
  [[_ {:keys [citizen-id]}]]
  [map/page-wrapper
   [citizen-view citizen-id]])

(pages/register-page!
  {:page/id :page/citizen
   :page/view #'page
   :page/path "/island/:island-id/citizen/:citizen-id"
   :page/parameters {:island-id :uuid
                     :citizen-id :uuid}
   :page/on-enter! (fn [[_ {:keys [island-id]}]]
                     (state/set-island-id! island-id))})
