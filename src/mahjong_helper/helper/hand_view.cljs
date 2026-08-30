(ns mahjong-helper.helper.hand-view
  (:require [mahjong-helper.helper.hand-tile :refer [hand-tile]]
            [re-re-frame.core :refer [subscribe]]))

(defn hand-view []
  (let [hand @(subscribe [:hand])
        melded-tiles @(subscribe [:melded-tile-idxs])]
    [:div#hand-view
     [:div {:style {:display "flex"
                    :justify-content "space-between"
                    :align-items "baseline"
                    :margin-bottom "6px"}}
      [:span "Your Hand"]
      [:span {:style {:color "#888" :font-size "13px"}}
       (str @(subscribe [:num-completed-tiles]) " / " @(subscribe [:hand-size]))]]
     [:div {:style {:display "flex"
                    :flex-wrap "wrap"
                    :gap "6px"
                    :min-height "58px"}}
      (doall
       (for [idx (range @(subscribe [:hand-size]))]
         (when-not (some #{idx} melded-tiles)
           (let [tile (get hand idx)]
             ^{:key idx} [hand-tile idx tile]))))]]))