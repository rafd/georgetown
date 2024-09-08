(ns georgetown.ui.dataviz
  (:require
    [georgetown.ui.common :as ui]))

(defn make-interpolator
  [[x0 x1] [y0 y1]]
  (let [m (/ (- y1 y0) (- x1 x0))
        b (- y0 (* m x0))]
    (fn [x]
      (+ (* m x) b))))

(defn sparkline
  [values]
  (let [width 60
        height 20
        y-range (apply max values)
        y-factor (/ height y-range)]
    [:svg {:style {:width (str width "px")
                   :height (str height "px")}}
     (for [[i value] (map-indexed vector values)]
       ^{:key i}
       [:rect {:fill (if (odd? i) "#9999ff" "#0000ff")
               :width 1
               :height (* y-factor value)
               :x i
               :y (- height (* y-factor value))}])]))

(defn plus-minus-sparkline
  [values]
  (let [bar-width 1
        indicator-height 3
        x-count (count values)
        #_#_values (take x-count (repeatedly (fn [] (rand-nth [-3 0 3]))))
        neg-color "#dc2626"
        zero-color "#333"
        pos-color "#2563eb"
        height 40
        width (* x-count bar-width)
        absmax (apply max (map abs values))
        ->x (make-interpolator [0 x-count] [0 width])
        ->y (make-interpolator [(- absmax) absmax]
                               [height 0])]
    [:div {:tw "flex justify-end py-4"}
     [:svg {:style {:width (str width "px")
                    :height (str height "px")}}
      [:rect {:fill "#ccc"
              :height 1
              :width width
              :x 0
              :y (/ height 2)}]
      (for [[i value] (map-indexed vector (reverse values))]
        ^{:key i}
        [:rect {:fill
                (case (compare value 0)
                  1 pos-color
                  0 zero-color
                  -1 neg-color)
                :width bar-width
                :height indicator-height
                :x (->x i)
                :y (- (->y value)
                      (case (compare value 0)
                        1 (- indicator-height)
                        0 1
                        -1 indicator-height))}])]
     [:div {:tw "flex flex-col justify-between items-end tabular-nums"
            :style {:font-size "0.35em"
                    :line-height 0}}
      [:div {:style {:color pos-color}} absmax]
      [:div {:style {:color neg-color}} (- absmax)]]]))

(defn multi-sparkline
  [& datasets]
  (let [bar-width 2
        bar-height 25
        x-range (count (first datasets))
        right-pad 0
        width (+ (* x-range bar-width) right-pad)
        height bar-height
        colors ["#0000ff" "#9999ff"]
        y-range (apply max (flatten datasets))
        y-factor (/ height y-range)]
    [:div {:tw "flex gap-2px"}
     [:svg {:style {:width (str width "px")
                    :height (str height "px")}}
      (for [[dataset-index values] (map-indexed vector (reverse datasets))]
        ^{:key dataset-index}
        [:g
         ;; reverse values so the latest is on the right
         (for [[i value] (map-indexed vector (reverse values))
               ;; sometimes get nils (ex. no clearing price)
               :let [value (or value 0)]]
           ^{:key i}
           [:rect {:fill (get colors (dec (- (count datasets) dataset-index)))
                   :width bar-width
                   :height (max 0 (* y-factor value))
                   :x (* i bar-width)
                   :y (- height (* y-factor value))}])])
      ;; placing labels at position in graph
      ;; overlapping, so for now, disable
      #_[:g
         (for [[dataset-index values] (map-indexed vector datasets)
               :let [value (first values)]]
           ^{:key dataset-index}
           [:text {:fill "black"
                   :font-size "10px"
                   :font-variant "tabular-nums"
                   :alignment-baseline "hanging"
                   :x (+ 2 (* bar-width x-range))
                   :y (- bar-height (* y-factor value))}
            (ui/format value 0)])]]
     (let [sig-figs (if (every? (fn [x] (< 1 x)) (map first datasets))
                      0
                      2)]
       [:div {:tw "flex flex-col text-right tabular-nums -mt-2px justify-between"}
        (for [[dataset-index value] (map-indexed vector (sort > (map first datasets)))]
          ^{:key dataset-index}
          [:div {:style {:font-size "0.5rem" :line-height 1}}
           (ui/format value sig-figs)])])]))

(defn bar-graph-view [& values]
  [:div {:tw "w-150px bg-white"}
   [:div.numbers {:tw "flex justify-between"}
    (for [[i value] (map-indexed vector values)]
      ^{:key i}
      [:div {:tw "text-blue"
             :style {:font-size "0.7em"}}
       (ui/format value 0)])]
   [:div.bars {:tw "relative h-1.5em"}
    (for [[i value] (map-indexed vector values)]
      ^{:key i}
      [:div {:style {:width (str (* 100 (/ value (last values))) "%")}
             :tw "absolute top-0 h-1em left-0 bottom-0 bg-#0000ff55"}])]])

(defn market-graph-view
  [demand tenders]
  (let [width 100
        height 20
        tenders (->> tenders
                     (map (fn [tender]
                            (-> tender
                                (assoc :tender/price
                                  (/ (get-in tender [:tender/demand 1])
                                     (get-in tender [:tender/supply 1]))))))
                     (sort-by :tender/price)
                     (sort-by (complement :tender/active?)))
        x-range (->> tenders
                     (map (fn [tender] (get-in tender [:tender/supply 1])))
                     (apply +))
        y-range (apply max (map :tender/price tenders))
        x-factor (/ width x-range)
        y-factor (/ height y-range)]
    [:div {:tw "flex relative items-end"
           :style {:width (str width "px")
                   :height (str height "px")}}
     (into [:<>]
           (for [tender tenders
                 :let [height (* y-factor (:tender/price tender))
                       width (* x-factor (get-in tender [:tender/supply 1]))]]
             [:div {:tw "shrink-0 grow-0 bg-gray-300 odd:bg-gray-400"
                    :style {:width (str width "px")
                            :height (str height "px")}}
              [:div {:tw "bg-#00ff0077"
                     :style {:margin-top (str (* height (- 1 (:tender/fill-ratio tender))) "px")
                             :height (str (* height (:tender/fill-ratio tender)) "px")
                             :width (str width "px")}}]]))
     [:div.line
      {:tw "absolute grow-0 shrink-0"
       :style {:width (str (* x-factor demand) "px")
               :left 0
               :height height
               :border-right "1px solid red"}}]]))
