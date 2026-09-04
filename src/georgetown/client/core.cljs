(ns ^:figwheel-hooks
  georgetown.client.core
  (:require
    [bloom.omni.reagent :as rdom]
    [bloom.commons.pages :as pages]
    [georgetown.client.ui.app :as app]
    [georgetown.client.state :as state]
    ;; pull in pages:
    [georgetown.client.ui.pages]))

(defn render []
  (rdom/render [app/app-view]))

(defn ^:export init []
  (pages/initialize! [])
  (render))

(defn ^:after-load reload
  []
  (render))

