(ns mahjong-helper.helper.modals.settings
  (:require [mahjong-helper.components.modal :refer [Modal]]
            [mahjong-helper.helper.settings :refer [settings]]
            [re-re-frame.core :refer [dispatch subscribe]]))

(defn settings-modal []
  [Modal {:open? @(subscribe [:settings-modal-open?])
          :title "Settings"
          :closable? true
          :on-close #(dispatch [:close-settings-modal])}
   [settings]
   [:div {:style {:font-size "0.1em"
                  :text-align :center
                  :color "gray"
                  :margin-top 20}}
    "2026-08-30"]
   [:button {:on-click #(dispatch [:close-settings-modal])}
    "Close"]])