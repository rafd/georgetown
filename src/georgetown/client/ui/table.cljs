(ns georgetown.client.ui.table
  (:require
    [reagent.core :as r]))

(def alignment->class
  {:alignment/left "text-left"
   :alignment/right "text-right tabular-nums"
   :alignment/center "text-center"})

(def opposite-direction
  {:direction/ascending :direction/descending
   :direction/descending :direction/ascending})

(def direction->indicator
  {:direction/ascending "▲"
   :direction/descending "▼"})

(defn compare-values
  [value-a value-b]
  (cond
    (= value-a value-b) 0
    (nil? value-a) -1
    (nil? value-b) 1
    :else (compare value-a value-b)))

(defn sort-rows
  [rows column direction]
  (if (nil? column)
    rows
    (cond->> (sort-by (:column/value column) compare-values rows)
      (= :direction/descending direction)
      reverse)))

(defn toggle-sort
  [current-sort column-key]
  (if (= column-key (:sort/column-key current-sort))
    (update current-sort :sort/direction opposite-direction)
    {:sort/column-key column-key
     :sort/direction :direction/ascending}))

(defn cell-class
  [column]
  ["px-2"
   (alignment->class (:column/alignment column :alignment/left))
   (:column/class column)])

(defn cell-content
  [column row]
  (if-let [render (:column/render column)]
    (render row)
    (when-let [value (:column/value column)]
      (value row))))

(defn header-cell
  [column sort-state]
  (let [sortable? (some? (:column/value column))]
    [:th {:tw ["px-2 font-normal whitespace-nowrap"
               (alignment->class (:column/alignment column :alignment/left))
               (when sortable?
                 "cursor-pointer select-none")]
          :title (:column/title column)
          :on-click (when sortable?
                      (fn [_]
                        (swap! sort-state toggle-sort (:column/key column))))}
     (:column/label column)
     (when (= (:column/key column) (:sort/column-key @sort-state))
       [:span {:tw "ml-1"}
        (direction->indicator (:sort/direction @sort-state))])]))

(defn sortable-table
  [{:keys [table/caption
           table/columns
           table/rows
           table/row-key
           table/row-attributes
           table/initial-sort]}]
  (r/with-let [sort-state (r/atom initial-sort)]
    (let [{:keys [sort/column-key sort/direction]} @sort-state
          sorted-column (->> columns
                             (filter (fn [column]
                                       (= column-key (:column/key column))))
                             first)]
      [:table
       (when caption
         [:caption {:tw "text-left text-sm text-gray-500"}
          caption])
       [:thead
        [:tr {:tw "text-xs text-gray-500"}
         (doall
           (for [column columns]
             ^{:key (:column/key column)}
             [header-cell column sort-state]))]]
       [:tbody
        (doall
          (for [row (sort-rows rows sorted-column direction)]
            ^{:key (row-key row)}
            [:tr (if row-attributes
                   (row-attributes row)
                   {})
             (doall
               (for [column columns]
                 ^{:key (:column/key column)}
                 [:td {:tw (cell-class column)}
                  (cell-content column row)]))]))]
       (when (some :column/footer columns)
         [:tfoot
          [:tr {:tw "font-bold"}
           (doall
             (for [column columns]
               ^{:key (:column/key column)}
               [:td {:tw (cell-class column)}
                (when-let [footer (:column/footer column)]
                  (footer rows))]))]])])))
