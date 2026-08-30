(ns mahjong-helper.helper.keyboard
  (:require [mahjong-helper.const :refer [suits tile-keys]]
            [mahjong-helper.utils :refer [dragon?]]
            [re-re-frame.core :refer [dispatch subscribe]]))

(defn keyboard []
  (let [editing @(subscribe [:editing])
        editing-dragon? (dragon? editing)]
    [:div {:style {:display "grid"
                   :grid-template-columns "repeat(4, 1fr)"
                   :gap "6px"}}
     (doall
      (for [item tile-keys]
        (let [{:keys [key icon on-click suit? style disabled?]} (if (string? item)
                                                                  {:key item}
                                                                  item)
              disabled? (if-not (nil? disabled?)
                          disabled?
                          (let [tile-that-would-be-created-if-button-was-pressed (assoc editing (if suit? :suit :value) key)]
                            @(subscribe [:creating-tile-disallowed? tile-that-would-be-created-if-button-was-pressed])))]
          [:button.tile {:key (str (when suit? "suit-") key)
                         :on-click #(when-not disabled?
                                      (if on-click
                                        (on-click)
                                        (dispatch [(if suit? :key-suit :key-value) key])))
                         :style (if suit?
                                  {:background (get-in suits [key :color])}
                                  style)
                         :class (when disabled? "keyboard-btn-disabled")
                         :disabled disabled?}
           [:div {:style {:visibility (when (and suit? editing-dragon?) :hidden)}}
            (cond
              (fn? icon)
              [icon]
              icon
              [:img {:src @(subscribe [:svg-url icon])
                     :draggable false}]
              :else
              key)]
           (when suit?
             [:div.dragon-name {:style {:visibility (when-not editing-dragon? :hidden)}}
              (get-in suits [key :dragon-name])])])))
     [:button.tile.tile-sm {:on-click #(dispatch [:edit-prev-tile])
                            :style {:background "black"
                                    :color "white"
                                    :grid-column "1 / 3"}}
      [:span.material-symbols-outlined "chevron_backward"]]
     [:button.tile.tile-sm {:on-click #(dispatch [:edit-next-tile])
                            :style {:background "black"
                                    :color "white"
                                    :grid-column "3 / 5"}}
      [:span.material-symbols-outlined "chevron_forward"]]]))