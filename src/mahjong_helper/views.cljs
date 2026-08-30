(ns mahjong-helper.views
  (:require [clojure.string :as string]
            [mahjong-helper.components.menu-btn :refer [Menu-Btn]]
            [mahjong-helper.const :refer [patterns]]
            [mahjong-helper.helper.modals.charleston :refer [charleston-modal]]
            [mahjong-helper.helper.modals.meld :refer [meld-modal]]
            [mahjong-helper.helper.modals.reset :refer [reset-modal]]
            [mahjong-helper.helper.modals.result :refer [result-modal]]
            [mahjong-helper.helper.modals.settings :refer [settings-modal]]
            [mahjong-helper.helper.modals.starting :refer [starting-modal]]
            [mahjong-helper.helper.hand-view :refer [hand-view]]
            [mahjong-helper.helper.keyboard :refer [keyboard]]
            [mahjong-helper.helper.melded-view :refer [melded-view]]
            [mahjong-helper.helper.results :refer [results-view]]
            [mahjong-helper.helper.solver :refer [find-arrangements]]
            [mahjong-helper.utils :refer [dragon?]]
            [re-re-frame.core :refer [dispatch subscribe]]
            [reagent.core :as r]))

(defn window-click
  [e]
  (let [el (.-target e)
        parents (reduce (fn [all-els _]
                          (if (nil? (.-parentElement (last all-els)))
                            all-els
                            (conj all-els (.-parentElement (last all-els)))))
                        [el]
                        (range 10))]
    (when-not (some #(or (= (.-tagName %) "BUTTON")
                         (= (.-tagName %) "A")
                         (= (.-className %) "modal")
                         (= (.-className %) "backdrop")) parents)
      (dispatch [:edit-tile -1]))))

(defn window-keydown
  [e]
  (let [key (string/lower-case (.-key e))
        editing @(subscribe [:editing])]
    (when (> @(subscribe [:editing-idx]) -1)
      (cond
        ;; 1–9
        (and (int? (js/Number.parseInt key))
             (> (js/Number.parseInt key) 0))
        (dispatch [:key-value key])
        ;; 0 = Soap
        (and (= key "0") 
             (not @(subscribe [:creating-tile-disallowed? {:value "D" :suit "D"}])))
        (do (dispatch [:key-value "D"])
            (dispatch [:key-suit "D"]))
        ;; B = Bamb
        (= key "b")
        (dispatch [:key-suit "B"])
        ;; C = Crak
        (= key "c")
        (dispatch [:key-suit "C"])
        ;; D = Dot (if value entered)
        (and (:value editing) (= key "d"))
        (dispatch [:key-suit "D"])
        ;; D = Dragon
        (= key "d")
        (dispatch [:key-value "D"])
        ;; E = East
        (= key "e")
        (dispatch [:key-value "E"])
        ;; F = Flower
        (= key "f")
        (dispatch [:key-value "F"])
        ;; G = green (when Dragon entered)
        (and (dragon? editing) (= key "g"))
        (dispatch [:key-suit "B"])
        ;; J = Joker
        (= key "j")
        (dispatch [:key-value "J"])
        ;; N = News
        (= key "n")
        (dispatch [:key-value "N"])
        ;; R = red (when Dragon entered)
        (and (dragon? editing) (= key "r"))
        (dispatch [:key-suit "C"])
        ;; S = soap (when Dragon entered)
        (and (dragon? editing) (= key "s"))
        (dispatch [:key-suit "D"])
        ;; S = South
        (= key "s")
        (dispatch [:key-value "S"])
        ;; W = West
        (= key "w")
        (dispatch [:key-value "W"])
        ;; Backspace
        (= key "backspace")
        (dispatch [:backspace])
        ;; Arrow left
        (= key "arrowleft")
        (dispatch [:edit-prev-tile])
        ;; Arrow right
        (= key "arrowright")
        (dispatch [:edit-next-tile])))))

(defn Main []
  (r/create-class
   {:component-did-mount
    (fn []
      (js/window.addEventListener "click" window-click)
      (js/window.addEventListener "keydown" window-keydown))
    :component-will-unmount
    (fn []
      (js/window.removeEventListener "click" window-click)
      (js/window.removeEventListener "keydown" window-keydown))
    :reagent-render
    (fn []
      [:<>
       [:div {:style {:max-width "480px"
                      :margin "0 auto"
                      :padding "12px 12px 36px"}}
        [starting-modal]
        [charleston-modal] 
        [meld-modal]
        [reset-modal]
        [settings-modal]
        [result-modal]
        [:div.title
         [:div {:style {:padding "10px"}}
          [Menu-Btn]
          "MAHJONG HELPER"
          [:div.title-right
           [:button.btn-orange {:style {:font-size "0.8rem"}
                                :on-click #(dispatch [:open-settings-modal])}
            "Settings"]
           [:button.clear-btn {:on-click #(dispatch [:open-reset-modal])}
            "Reset"]]]]
        [:div {:style {:height "calc(1.5em + 20px)"}}]
        [melded-view]
        [hand-view]
        (when @(subscribe [:hand-complete?])
          [:<>
           [:div.charleston
            [:button.btn-orange.arrow-btn {:on-click #(dispatch [:open-charleston-modal])}
             "Charleston"]]
           
           [:div.meld
            [:button.btn-purple.arrow-btn {:on-click #(dispatch [:open-meld-modal])}
             "Meld"]]])
        [results-view]
        [:div {:style {:visibility :hidden}}
         (when (>= @(subscribe [:editing-idx]) 0)
           [keyboard])]]
       (when (>= @(subscribe [:editing-idx]) 0)
         [:div.keyboard
          [keyboard]])])}))

(comment
  (defn data-validation []
    [:<>
     (doall
      (for [[pattern {:keys [id category]}] (->> patterns
                                                 (sort-by #(-> % last :id)))]
        [:div {:style {:display :flex :align-items :center  :margin-bottom "1em"}}
         [:code {:style {:color "gray" :font-size "0.8em" :width "200px" :text-align :right :margin-right "1em"}} "#" id " - " category]
         [display-pattern pattern]]))])
  (find-arrangements "2F.4ra4sb4tc" @(subscribe [:hand-as-strs]))
  )