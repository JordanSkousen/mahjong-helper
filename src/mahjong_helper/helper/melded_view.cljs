(ns mahjong-helper.helper.melded-view
  (:require [mahjong-helper.helper.hand-tile :refer [hand-tile]]
            [re-re-frame.core :refer [subscribe]]))

(defn melded-view []
  (let [meld-groups @(subscribe [:meld-groups])] 
    (when (seq meld-groups)
      (let [hand @(subscribe [:hand])]
        [:div {:style {:margin-bottom 10}}
         [:div {:style {:display "flex"
                        :justify-content "space-between"
                        :align-items "baseline"
                        :margin-bottom "6px"}}
          [:span "Melded Tiles"]]
         [:div {:style {:display "flex"
                        :flex-wrap "wrap"
                        :gap "6px"
                        :min-height "58px"}}
          (doall
           (for [[id meld-group] meld-groups]
             [:div {:key id
                    :style {:display "flex"
                            :flex-wrap "wrap"
                            :gap "3px"
                            :margin-right 10}}
              (doall
               (for [idx meld-group]
                 (let [tile (get hand idx)]
                   ^{:key idx} [hand-tile idx tile :melded])))]))]]))))