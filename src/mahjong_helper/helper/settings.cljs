(ns mahjong-helper.helper.settings
  (:require [re-re-frame.core :refer [dispatch subscribe]]))

(defn settings []
  (let [theme @(subscribe [:theme])
        starting-player? @(subscribe [:starting-player?])]
    [:div {:style {:font-size "1.2em"}}
     [:div {:style {:display :flex
                    :align-items :centfr}}
      "Are you starting player?"
      [:div.button-group {:style {:margin-left 10
                                  :display :inline-flex
                                  :flex "1 0"}}
       [:button.btn-sm {:class (when starting-player? "button-active")
                        :on-click #(dispatch [:starting-player true])}
        "Yes"]
       [:button.btn-sm {:class (when-not starting-player? "button-active")
                        :on-click #(dispatch [:starting-player false])}
        "No"]]]
     [:div {:style {:margin-top 10}}
      "Theme: "
      [:div.button-group {:style {:display :inline-flex
                                  :width :fit-content}}
       (doall
        (for [[key label] {:jordan "Jordan's"
                           :traditional "Traditional"}]
          [:button.btn-sm {:class (when (= theme key) "button-active")
                           :on-click #(dispatch [:theme key])}
           label]))]]]))