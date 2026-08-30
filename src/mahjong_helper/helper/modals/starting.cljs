(ns mahjong-helper.helper.modals.starting
  (:require [mahjong-helper.components.modal :refer [Modal]]
            [mahjong-helper.helper.settings :refer [settings]]
            [re-re-frame.core :refer [dispatch subscribe]]))

(defn starting-modal []
  [Modal {:open? @(subscribe [:starting-modal-open?])
          :title "Settings"}
   [settings]
   [:hr]
   [:button {:style {:background "rgb(10, 159, 17)"
                     :color "white"}
             :on-click #(dispatch [:close-starting-modal])}
    "Start"]])