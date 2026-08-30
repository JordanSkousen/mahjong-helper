(ns mahjong-helper.helper.hand-tile
  (:require [mahjong-helper.const :refer [suits tile-keys]]
            [mahjong-helper.utils :refer [dragon? suitless?]]
            [re-re-frame.core :refer [dispatch subscribe]]))

(defn hand-tile-inner
  [idx tile type]
  (let [{:keys [value suit]} tile
        type (or type :normal)
        normal? (= type :normal)
        editing? (and normal?
                      (= idx @(subscribe [:editing-idx])))
        soap? (and (dragon? tile) (= suit "D"))]
    (if soap?
      [:img {:src @(subscribe [:svg-url 0])
             :height "36px"
             :draggable false}]

      [:<>
       (let [{:keys [icon]} (->> tile-keys
                                 (filter #(and (= (:key %) value)
                                               (not (:suit? %))))
                                 first)]
         (cond
           (fn? icon)
           [icon {:fill (if (or editing? (not suit)) "#000" "#fff")}]
           icon
           [:img {:src @(subscribe [:svg-url icon])
                  :height "36px"
                  :draggable false}]
           :else
           (or value "·")))
       (when (and suit (not (dragon? tile)))
         [:img.suit-indicator {:src @(subscribe [:svg-url (get-in suits [suit :icon])])
                               :draggable false}])])))

(defn hand-tile-style
  [idx tile type]
  (let [{:keys [suit]} tile
        type (or type :normal)
        normal? (= type :normal)
        editing? (and normal?
                      (= idx @(subscribe [:editing-idx])))
        soap? (and (dragon? tile) (= suit "D"))]
    {:background (when-not soap?
                   (cond
                     (and suit editing?) (get-in suits [suit :color-light])
                     suit (get-in suits [suit :color])
                     :else "#fff"))
     :color (if (and suit (not editing?)) "white" "black")}))

(defn hand-tile
  "A completed tile in the hand; tap to edit it."
  [idx tile type]
  (let [{:keys [value suit]} tile
        type (or type :normal)
        normal? (= type :normal)
        charleston? (= type :charleston)
        melded? (= type :melded)
        needs-suit? (not (suitless? value))
        editing? (and normal?
                      (= idx @(subscribe [:editing-idx])))
        charleston-selected? (and charleston?
                                  (get @(subscribe [:charleston-selection]) idx))]
    [:button.tile.hand-tile {:on-click #(cond
                                          charleston?
                                          (dispatch [:toggle-charleston-select idx])
                                          melded?
                                          (dispatch [:open-meld-modal])
                                          :else
                                          (do (dispatch [:edit-tile idx])
                                              (js/window.scrollTo #js{:top 0 :behavior "smooth"})))
                             :class [(when (and (not editing?)
                                                (or (not value)
                                                    (and needs-suit? (not suit))))
                                       "pending-tile")
                                     (when (suitless? value)
                                       "suitless-tile")
                                     (when editing?
                                       "editing-tile")
                                     (when charleston-selected?
                                       "charleston-selected-tile")
                                     (when melded?
                                       "melded-tile")]
                             :style (hand-tile-style idx tile type)}
     [hand-tile-inner idx tile type]]))