(ns georgetown.client.ui.pages.citizens
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.sim.time :as time]
    [georgetown.client.ui.common :as ui]
    [georgetown.client.state :as state]
    [georgetown.client.ui.map :as map]))

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

(defn citizen-view
  [citizen]
  [:div
   [:h3 {:tw "text-sm font-bold"}
    [ui/resource-icon :resource/citizen] " " (subs (str (:citizen/id citizen)) 0 8)]
   [:div {:tw "flex gap-2"}
    [:div
     [:h4 {:tw "text-xs font-bold"} "Constants"]
     [stat-table citizen constant-stats]]
    [:div
     [:h4 {:tw "text-xs font-bold"} "Variables"]
     [stat-table citizen variable-stats]]]])

(defn citizens-view []
  (let [citizens (:island/citizens @state/island)]
    [:section
     [:h1 "Citizens"]
     [:p (count citizens) " citizens"]
     [:div {:tw "space-y-2"}
      (doall
        (for [citizen citizens]
          ^{:key (:citizen/id citizen)}
          [citizen-view citizen]))]]))

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
