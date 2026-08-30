(ns mahjong-helper.helper.modals.result
  (:require [mahjong-helper.components.modal :refer [Modal]]
            [mahjong-helper.const :refer [patterns]]
            [mahjong-helper.helper.arrangement :refer [arrangement-view]]
            [mahjong-helper.helper.display-pattern :refer [display-pattern]]
            [mahjong-helper.helper.solver :refer [find-arrangements]]
            [re-re-frame.core :refer [dispatch subscribe]]))

(defn result-modal []
  (let [pattern @(subscribe [:result-modal-open-pattern])
        hand-as-strs @(subscribe [:hand-as-strs])
        melds @(subscribe [:meld-groups-as-strs])
        {:keys [id category]} (get patterns pattern)
        arrangements (when pattern (find-arrangements pattern hand-as-strs melds))]
    [Modal {:open? (some? pattern)
            :title [:div {:style {:font-weight :normal}}
                    [:div.result-modal-top
                     [:span.chip category]
                     (when-not @(subscribe [:traditional-theme?])
                       [:code {:style {:color :gray
                                       :font-weight :normal}}
                        "#" id " "])]
                    [:div.result-modal-pattern
                     [display-pattern pattern true]]]
            :closable? true
            :on-close #(dispatch [:open-result-modal nil])}
     [:div.result-modal
      (when pattern
        (if (<= (count arrangements) 1)
          [arrangement-view pattern (first arrangements)]
          (doall
           (for [[i arr] (map-indexed vector arrangements)]
             ^{:key i}
             [:div
              [:div {:style {:font-weight 700
                             :margin-top (when (pos? i) "16px")
                             :color "#666"
                             :font-size "0.7em"}}
               (str "Option " (inc i))]
              [arrangement-view pattern arr]]))))
      [:button {:style {:position :sticky
                        :bottom 0}
                :on-click #(dispatch [:open-result-modal nil])}
       "Close"]]]))