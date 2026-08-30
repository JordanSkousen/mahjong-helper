(ns mahjong-helper.helper.results
  (:require [mahjong-helper.const :refer [patterns]]
            [mahjong-helper.helper.arrangement :refer [arrangement-view]]
            [mahjong-helper.helper.display-pattern :refer [display-pattern]]
            [mahjong-helper.helper.worker-pool :refer [rank-patterns-async]]
            [re-re-frame.core :refer [dispatch subscribe]]
            [reagent.core :as r]))

;; Ranking every pattern against a full hand is a somewhat expensive
;; backtracking search (~130 patterns), so it's farmed out to a pool of
;; Web Workers (see worker-pool.cljs) instead of blocking the render.
;; :hand tracks which hand the current :rankings were computed for, so
;; a stale in-flight computation can be told apart from a fresh one.

;; TODO: move these to re-frame and don't do it the stupid way AI decided on
(defonce ^:private pattern-rankings (r/atom nil))
(defonce ^:private rankings-computed-for (atom nil))

(defn- ensure-rankings!
  [hand-as-strs melds]
  (let [computed-for [hand-as-strs melds]]
    (when (not= @rankings-computed-for computed-for)
      (reset! rankings-computed-for computed-for)
      (rank-patterns-async hand-as-strs melds
                           (fn [results]
                             ;; drop the result if a newer hand/melds has since superseded it
                             (when (= @rankings-computed-for computed-for)
                               (reset! pattern-rankings {:hand hand-as-strs :rankings results})))))))

(defn results-view []
  (let [hand-as-strs @(subscribe [:hand-as-strs])
        melds @(subscribe [:meld-groups-as-strs])
        preview-mode? @(subscribe [:preview-mode?])
        traditional-theme? @(subscribe [:traditional-theme?])]
    (when @(subscribe [:hand-complete?])
      (ensure-rankings! hand-as-strs melds)
      (let [{:keys [hand rankings]} @pattern-rankings
            computing? (not= hand hand-as-strs)]
        [:<>
         [:hr {:style {:margin-top "20px"
                       :margin-left "-20px"
                       :margin-right "-20px"}}]
         [:div {:style {:margin-top "16px"}}
          [:div {:style {:margin-bottom "6px"}}
           [:span "Closest Mahjongs"]
           (when computing?
             [:span {:style {:margin-left "8px" :color "#888" :font-size "0.7em"}}
              "Calculating…"])
           [:span {:style {:float :right}
                   :on-click #(dispatch [:toggle-preview-mode])}
            [:div.switch {:class (when preview-mode? "switch-on")}
             [:div.switch-knob]]
            " Preview Mode"]]
          [:div {:style {:opacity (when computing? 0.5)}}
           (doall
            (for [{:keys [id pattern ranking arrangement]}
                  (->> rankings
                       (map (fn [[pattern {:keys [ranking arrangement]}]]
                              {:id (get-in patterns [pattern :id])
                               :pattern pattern
                               :ranking ranking
                               :arrangement arrangement}))
                       ;; a ranking of 0 means impossible (e.g. a meld
                       ;; that doesn't fit anywhere in this pattern) —
                       ;; not merely unmatched-so-far, so it's not worth
                       ;; showing at all
                       (remove #(zero? (:ranking %)))
                       (sort-by :id)
                       (sort-by :ranking >))
                  :let [ranking-pct (/ ranking 14) ;; this should always be 14 since you need 14 tiles to get mahjong, duh
                        ]] 
              ^{:key pattern}
              [:div.pattern {:on-click #(dispatch [:open-result-modal pattern])}
               (when-not traditional-theme?
                 [:code.pattern-id "#" id])
               [:div.pattern-container
                (if preview-mode?
                  ;; precomputed in the worker alongside the ranking, so
                  ;; toggling Preview Mode doesn't force a synchronous
                  ;; find-arrangements pass over every pattern
                  [arrangement-view pattern arrangement]
                  [display-pattern pattern])
                [:div.pattern-percent
                 [:div {:style {:width (str (max 1 (* ranking-pct 100)) "%")
                                :background (str "hsl(" (* ranking-pct 120) ", 73%, 41%)")}}]
                 [:span (/ (js/Math.floor (* ranking-pct 1000)) 10) "%"]]]
               [:span.pattern-view {:style {:color "rgb(0, 166, 255)"}}
                [:span.material-symbols-outlined "visibility"]]]))]]]))))