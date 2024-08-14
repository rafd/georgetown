(ns georgetown.ui.pages.home
  (:require
    [reagent.core :as r]
    [bloom.commons.pages :as pages]
    [georgetown.client.state :as state]))

(defn view []
  (r/with-let [islands (state/exec-atom! :query/islands {})]
    [:div
     "Islands:"
     (for [island @islands]
       ^{:key (:island/id island)}
       [:a
        {:href (pages/path-for [:page/island {:island-id (:island/id island)}])}
        (str (:island/id island))])]))

(pages/register-page!
  {:page/id :page/home
   :page/view #'view
   :page/path "/"})
