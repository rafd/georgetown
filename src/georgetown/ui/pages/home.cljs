(ns georgetown.ui.pages.home
  (:require
    [bloom.commons.pages :as pages]))

(defn view []
  [:div
   "home"])

(pages/register-page!
  {:page/id :page/home
   :page/view #'view
   :page/path "/"})
