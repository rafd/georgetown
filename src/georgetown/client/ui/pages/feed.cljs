(ns georgetown.client.ui.pages.feed
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.client.state :as state]
    [georgetown.client.ui.events :as events]
    [georgetown.client.ui.map :as map]))

(defn event-row [event]
  [:div {:tw "text-sm flex gap-2"}
   [:span {:tw "tabular-nums text-gray-500 shrink-0"}
    (map/date (:event/epoch event))]
   [:span
    (when (= :visibility/limited (:event/visibility event))
      {:tw "italic"})
    [events/render event]]])

(defn feed-view []
  (let [events (->> (concat (:island/events @state/island)
                            (:player/events @state/player))
                    (sort-by (juxt :event/epoch
                                   (fn [event]
                                     (str (:event/id event)))))
                    reverse)]
    [:section
     [:h1 "Feed"]
     [:div {:tw "space-y-1"}
      (doall
        (for [event events]
          ^{:key (:event/id event)}
          [event-row event]))]]))

(defn page [_]
  [map/page-wrapper
   [feed-view]])

(pages/register-page!
  {:page/id :page/feed
   :page/view #'page
   :page/path "/island/:island-id/feed"
   :page/parameters {:island-id :uuid}
   :page/on-enter! (fn [[_ {:keys [island-id]}]]
                     (state/set-island-id! island-id))})
