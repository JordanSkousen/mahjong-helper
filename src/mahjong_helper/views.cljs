(ns mahjong-helper.views
  (:require [clojure.string :as string]
            [mahjong-helper.const :refer [suits tile-keys closed-pattern? WILDS1 WILDS2 all-tiles]]
            [mahjong-helper.modal :refer [Modal]]
            [mahjong-helper.solver :refer [pattern-groups rank-patterns]]
            [mahjong-helper.utils :refer [tile-complete? tile-map->str suitless? dragon?]]
            [re-re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as r]))

(defn hand-tile
  "A completed tile in the hand; tap to edit it."
  [idx tile charleston?]
  (let [{:keys [value suit]} tile
        needs-suit? (not (suitless? value))
        editing? (and (not charleston?)
                      (= idx @(subscribe [:editing-idx])))
        charleston-selected? (and charleston?
                                  (get @(subscribe [:charleston-selection]) idx))
        soap? (and (dragon? tile) (= suit "D"))]
    [:button.tile.hand-tile {:on-click #(if charleston?
                                          (dispatch [:toggle-charleston-select idx])
                                          (do (dispatch [:edit-tile idx])
                                              (js/window.scrollTo #js{:top 0 :behavior "smooth"})))
                             :class [(when (and (not editing?)
                                                (or (not value)
                                                    (and needs-suit? (not suit))))
                                       "pending-tile")
                                     (when editing?
                                       "editing-tile")
                                     (when charleston-selected?
                                       "charleston-selected-tile")]
                             :style {:border-color (if editing? "#f1b81a" "#ccc")
                                     :background (when-not soap?
                                                   (cond
                                                     (and suit editing?) (get-in suits [suit :color-light])
                                                     suit (get-in suits [suit :color])
                                                     :else "#fff"))
                                     :color (if (and suit (not editing?)) "white" "black")}}
     (if soap?
       [:img {:src "/img/0.svg"
              :height "36px"}]
       
       [:<>
        (let [{:keys [icon]} (->> tile-keys
                                  (filter #(= (:key %) value))
                                  first)]
          (cond 
            (fn? icon)
            [icon {:fill (if (or editing? (not suit)) "#000" "#fff")}]
            icon
            [:img {:src (str "/img/" icon ".svg")
                   :height "36px"}]
            :else
            (or value "·")))
        (when (and suit (not (dragon? tile)))
          [(get-in suits [suit :icon]) (if (and suit (not editing?)) "#fff" "#000")])])]))

(defn hand-view []
  (let [hand @(subscribe [:hand])]
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
         (let [tile (get hand idx)]
           ^{:key idx} [hand-tile idx tile])))]]))

(defn keyboard []
  (let [hand-size @(subscribe [:hand-size])
        editing-idx @(subscribe [:editing-idx])
        hand-as-strs (->> (dissoc @(subscribe [:hand]) editing-idx)
                          vals
                          (map tile-map->str))
        remaining-tile-counts (reduce (fn [all-tiles' tile-str]
                                        (update all-tiles' tile-str dec))
                                      all-tiles
                                      hand-as-strs)
        editing @(subscribe [:editing])
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
                          (let [tile-that-would-be-created-if-button-was-pressed (assoc editing (if suit? :suit :value) key)
                                ttwbcibwp-str (tile-map->str tile-that-would-be-created-if-button-was-pressed)]
                            (when-not (nil? (get remaining-tile-counts ttwbcibwp-str))
                              (<= (get remaining-tile-counts ttwbcibwp-str) 0))))]
          [:button.tile {:key key
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
              [:img {:src (str "/img/" icon ".svg")}]
              :else
              key)]
           (when suit?
             [:div.dragon-name {:style {:visibility (when-not editing-dragon? :hidden)}}
              (get-in suits [key :dragon-name])])])))
     [:button.tile.tile-sm {:on-click #(dispatch [:edit-tile (if (= editing-idx 0)
                                                               (dec hand-size)
                                                               (dec editing-idx))])
                            :style {:background "black"
                                    :color "white"
                                    :grid-column "1 / 3"}}
      [:span.material-symbols-outlined "chevron_backward"]]
     [:button.tile.tile-sm {:on-click #(dispatch [:edit-tile (if (= editing-idx (dec hand-size))
                                                               0
                                                               (inc editing-idx))])
                            :style {:background "black"
                                    :color "white"
                                    :grid-column "3 / 5"}}
      [:span.material-symbols-outlined "chevron_forward"]]]))

(defn display-group
  [s]
  (let [[mul val suit] s
        color (case suit
                "a" "#d18e29"
                "b" "#6fc7b3"
                "c" "#6a52a2"
                "black")]
    [:span.group {:style {:color color}}
     (repeat (int mul)
             (cond
               ;; suitless icon
               (some #{val} ["F" "N" "E" "W" "S" "0"])
               [:img {:src (str "/img/" val ".svg")
                      :class (when (= val "0") "white-dragon")
                      :alt val}]
               
               ;; suited icon (dragon)
               (= val "D")
               [(->> tile-keys
                     (filter #(= (:key %) "Dragon"))
                     first
                     :icon) {:fill color}]

               ;; WILDS1 sequential letter
               (string/includes? WILDS1 val)
               (js/String.fromCharCode (- (.charCodeAt val 0) 49))
               
               ;; WILDS2 
               (string/includes? WILDS2 val)
               "#"
               
               :else
               val))]))

(defn display-pattern
  [pattern]
  (let [split (string/split pattern #"\(|\)")]
    [:div
     (->> split
          (map-indexed (fn [idx s]
                         (when-not (string/blank? s)
                           ^{:key idx}
                           [:span (->> s
                                       pattern-groups
                                       (map (fn [group]
                                              ^{:key group}
                                              [:span (display-group group) (when (even? idx) " ")]))) " "]))))
     (when (closed-pattern? pattern)
       [:img.closed {:src "/img/Closed.svg"
                     :alt "CLOSED"}])]))

(def rank-patterns-memo (memoize rank-patterns))
(defn results-view []
  (let [hand-as-strs @(subscribe [:hand-as-strs])]
    (when @(subscribe [:hand-complete?])
      [:<>
       [:hr {:style {:margin-top "20px"
                     :margin-left "-20px"
                     :margin-right "-20px"}}]
       [:div {:style {:margin-top "16px"}} 
        [:div {:style {:margin-bottom "6px"}} "Closest Mahjongs"]
        (for [[pattern ranking] (->> (rank-patterns-memo hand-as-strs)
                                     (sort-by val >))]
          ^{:key pattern}
          [:div.pattern {:style {:display "flex"
                                 :align-items "center"
                                 :gap "10px"
                                 :padding "6px 0"
                                 :border-bottom "1px solid #eee"}}
           [:span.ranking (/ (js/Math.floor (* (/ ranking 14) 1000)) 10) "%"]
           [display-pattern pattern]])]])))

(defn starting-player-modal [] 
  [Modal {:open? (nil? @(subscribe [:starting-player?]))
          :title "Are you starting player?"}
   [:button {:style {:color "green"}
             :on-click #(dispatch [:starting-player true])}
    "Yes"]
   [:button {:on-click #(dispatch [:starting-player false])}
    "No"]])

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
           ^{:key idx} [hand-tile idx tile true])))]
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
    [:button {:on-click #(dispatch [:initialize-db])}
     "Yes, Reset"]]])

(defn Main []
  (r/create-class
   {:component-did-mount
    (fn []
      (js/window.addEventListener "click" (fn [e]
                                            (let [el (.-target e)
                                                  parents (reduce (fn [all-els _]
                                                                    (if (nil? (.-parentElement (last all-els)))
                                                                      all-els
                                                                      (conj all-els (.-parentElement (last all-els)))))
                                                                  [el]
                                                                  (range 10))]
                                              (when-not (some #(or (= (.-tagName %) "BUTTON")
                                                                   (= (.-tagName %) "A")) parents)
                                                (dispatch [:edit-tile -1]))))))
    :reagent-render
    (fn []
      [:<>
       [:div {:style {:max-width "480px"
                      :margin "0 auto"
                      :padding "12px 12px 36px"}}
        [starting-player-modal]
        [charleston-modal]
        [reset-modal]
        [:div.title
         [:div {:style {:padding "10px"}}
          "MAHJONG HELPER"
          [:div.title-right
           [:button.clear-btn {:on-click #(dispatch [:open-reset-modal])}
            "Reset"]]]]
        [:div {:style {:height "calc(1.5em + 20px)"}}]
        [hand-view]
        (when @(subscribe [:hand-complete?])
          [:div.charleston
           [:button.arrow-btn {:style {:background "rgb(243, 142, 26)"}
                               :on-click #(dispatch [:open-charleston-modal])}
            "Charleston"]])
        [results-view]
        [:div {:style {:visibility :hidden}}
         (when (>= @(subscribe [:editing-idx]) 0)
           [keyboard])]]
       (when (>= @(subscribe [:editing-idx]) 0)
         [:div.keyboard
          [keyboard]])])}))