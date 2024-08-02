(ns georgetown.ui.app
  (:require
    [bloom.commons.pages :as pages]))

(defn app-view []
  [:div
   "welcome to georgetown"
   [pages/current-page-view]])
