(ns georgetown.ui.pages.island
  (:require
    [bloom.commons.pages :as pages]
    [georgetown.client.state :as state]
    [georgetown.ui.map :as map]))

(defn island-view
  [_]
  [map/page-wrapper
   [:div
    "Welcome to Georgetown!"]])

(pages/register-page!
  {:page/id :page/island
   :page/view #'island-view
   :page/path "/island/:island-id"
   :page/parameters {:island-id :uuid}
   :page/on-enter! (fn [[_ {:keys [island-id]}]]
                     (state/set-island-id! island-id))})
