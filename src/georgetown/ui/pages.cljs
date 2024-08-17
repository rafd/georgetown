(ns georgetown.ui.pages
  (:require
    [georgetown.ui.pages.home]
    [georgetown.ui.pages.bank]
    [georgetown.ui.pages.island]
    [georgetown.ui.pages.lot]
    [georgetown.ui.pages.finances]
    [georgetown.ui.pages.gazette]))

(when js/goog.DEBUG
  (require 'georgetown.ui.pages.debug))

