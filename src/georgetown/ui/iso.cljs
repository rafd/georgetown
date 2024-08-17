(ns georgetown.ui.iso
  (:require
    [reagent.core :as r]
    [georgetown.biome :as biome]
    [georgetown.client.state :as state]))

(defonce _
  (js/document.body.appendChild
    (doto (js/document.createElement "script")
      (aset "src" "https://cdn.jsdelivr.net/gh/elchininet/isometric@3.7.3/dist/web/isometric.js"))))

(def island-pattern
  [:island/id
   {:island/lots [:lot/x
                  :lot/y
                  :lot/elevation
                  :lot/moisture]}])

(defn iso-view
  ;; https://elchininet.github.io/isometric/
  [island]
  (r/with-let [iso-el (atom nil)
               draw! (fn []
                       (when js/window.isometric
                         ;; clear children
                         (aset @iso-el "innerHTML" "")
                         (let [lots-by-xy (zipmap
                                           (map (juxt :lot/x :lot/y) (:island/lots island))
                                           (:island/lots island))
                               water-color (biome/color {:lot/x 0
                                                         :lot/y 1
                                                         :lot/elevation 0.0
                                                         :lot/moisture 0.0})
                               canvas (js/isometric.IsometricCanvas.
                                       #js {:container @iso-el
                                            :backgroundColor water-color
                                            :scale 10
                                            :width 380
                                            :height 250})
                               x-offset -10
                               y-offset -10]
                           (doseq [x (range 20)
                                   y (range 20)
                                   :let [lot (lots-by-xy [(- 19 x) y])
                                         h (+ 1 (* 3 (:lot/elevation lot))
                                              (if (= :biome/water (biome/lot lot))
                                                0
                                                0.25))]]
                             (.addChildren
                              canvas
                              (.addChildren
                               (js/isometric.IsometricGroup.)
                               (js/isometric.IsometricRectangle.
                                #js {:height h
                                     :width 1
                                     :planeView js/isometric.PlaneView.FRONT
                                     :fillColor (biome/color (assoc lot :lot/side? true))
                                     :strokeWidth 0
                                     :left (+ 1 x x-offset)
                                     :right (+ 2 y y-offset)})
                               (js/isometric.IsometricRectangle.
                                #js {:height h
                                     :width 1
                                     :planeView js/isometric.PlaneView.SIDE
                                     :fillColor (biome/color (assoc lot :lot/side? true))
                                     :strokeWidth 0
                                     :left (+ 2 x x-offset)
                                     :right (+ 1 y y-offset)})
                               (js/isometric.IsometricRectangle.
                                #js {:height 1
                                     :width 1
                                     :planeView js/isometric.PlaneView.TOP
                                     :fillColor (biome/color lot)
                                     :strokeWidth 0
                                     :top (+ -1 h)
                                     :left (+ x x-offset)
                                     :right (+ y y-offset)}))))
                           ;; front water
                           (doseq [[left right] [[20 0] [20 20] [0 20]]]
                             (.addChild
                              canvas
                              (js/isometric.IsometricRectangle.
                               #js {:height 20
                                    :width 20
                                    :planeView js/isometric.PlaneView.TOP
                                    :fillColor water-color
                                    :strokeWidth 0
                                    :top 0
                                    :left (+ left x-offset)
                                    :right (+ right y-offset)}))))))]
              (when island
                [:div {:on-click (fn []
                                   (draw!))
                       :ref (fn [el]
                              (when el
                                (when (nil? @iso-el)
                                  (reset! iso-el el)
                                  (draw!))))}])))
