(ns georgetown.ui.pages.home
  (:require
    [bloom.commons.pages :as pages]))

(defn view []
  [:div
   "Welcome to Georgetown!"])

(pages/register-page!
  {:page/id :page/home
   :page/view #'view
   :page/path "/"})
