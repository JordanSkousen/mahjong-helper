(ns mahjong-helper.helper.modals.reset
  (:require [mahjong-helper.components.modal :refer [Modal]]
            [mahjong-helper.utils :refer [read-storage]]
            [re-re-frame.core :refer [dispatch subscribe]]))

(defn reset-modal []
  [Modal {:open? @(subscribe [:reset-modal-open?])
          :title "Are you sure?"
          :closable? true
          :on-close #(dispatch [:close-reset-modal])}
   "Are you sure you want to reset your hand?"
   [:div.buttons-row
    [:button {:style {:background "red"
                      :color "white"
                      :border :none}
              :on-click #(dispatch [:close-reset-modal])}
     "Cancel"]
    [:button {:on-click #(dispatch [:reset-game (read-storage true)])}
     "Yes, Reset"]]])