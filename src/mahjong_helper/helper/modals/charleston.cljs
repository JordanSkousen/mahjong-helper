(ns mahjong-helper.helper.modals.charleston
  (:require [mahjong-helper.components.modal :refer [Modal]]
            [mahjong-helper.helper.hand-tile :refer [hand-tile]]
            [re-re-frame.core :refer [dispatch subscribe]]))

(defn charleston-modal []
  (let [hand @(subscribe [:hand])]
    [Modal {:open? @(subscribe [:charleston-modal-open?])
            :title "Choose up to 3 tiles to pass"
            :closable? true
            :on-close #(dispatch [:close-charleston-modal])}
     [:div {:style {:display "flex"
                    :flex-wrap "wrap"
                    :gap "6px"
                    :min-height "58px"}}
      (doall
       (for [idx (range @(subscribe [:hand-size]))]
         (let [tile (get hand idx)]
           ^{:key idx} [hand-tile idx tile :charleston])))]
     [:div.buttons-row
      [:button {:style {:background "red"
                        :color "white"
                        :border :none}
                :on-click #(dispatch [:close-charleston-modal])}
       "Cancel"]
      [:button {:style {:background "green"
                        :color "white"
                        :border :none}
                :on-click #(dispatch [:save-charleston])}
       "Save"]]]))