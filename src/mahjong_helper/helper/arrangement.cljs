(ns mahjong-helper.helper.arrangement
  (:require [mahjong-helper.const :refer [suits tile-keys]]
            [mahjong-helper.helper.group-glyph :refer [group-glyph]]
            [mahjong-helper.helper.solver :refer [groups-with-slots
                                                  resolve-group-str]]
            [mahjong-helper.utils :refer [dragon?]]
            [re-re-frame.core :refer [subscribe]]))

(defn tile-face
  "Renders a concrete 2-char tile string (\"5B\", \"N.\", \"F.\", \"DB\",
   \"DD\" for soap...) the way hand-tile does. `muted?` shows it as a
   still-needed tile rather than one already in hand."
  [tile-str & [{:keys [muted?]}]]
  (let [value (subs tile-str 0 1)
        suit (let [s (subs tile-str 1 2)] (when (not= s ".") s))
        soap? (and (dragon? value) (= suit "D"))
        melded? (some #{tile-str} (apply concat @(subscribe [:meld-groups-as-strs])))
        {:keys [icon]} (->> tile-keys
                            (filter #(and (= (:key %) value)
                                          (not (:suit? %))))
                            first)]
    [:div.tile.hand-tile.tile-face {:class [(when muted? "tile-muted")
                                            (when melded? "melded-tile")]
                                    :style {:background (when-not soap?
                                                          (get-in suits [suit :color]))
                                            :color (if (and suit (not muted?)) "white" "black")}}
     (if soap?
       [:img {:src @(subscribe [:svg-url 0])
              :height "36px"}]
       [:<>
        (cond
          (fn? icon) [icon {:fill (if (or muted? (not suit)) "#000" "#fff")}]
          icon [:img {:src @(subscribe [:svg-url icon])
                      :height "36px"}]
          :else value)
        (when (and suit (not soap?))
          [:img.suit-indicator {:src @(subscribe [:svg-url (get-in suits [suit :icon])])}])])]))

(defn arrangement-view
  "One way to fill pattern's slots from the hand: matched slots show the
   actual tile, unfilled ones show what's needed (or a generic placeholder
   if this arrangement hasn't pinned down which suit/wild number is needed)."
  [pattern {:keys [context assignment]}]
  [:div.arrangement-view {:style {:display "flex" :flex-wrap "wrap" :gap "3px" :margin "10px 0"}}
   (doall
    (for [{:keys [group tiles]} (groups-with-slots pattern assignment)]
      ^{:key group}
      [:div {:style {:display "flex" :gap "3px"}}
       (doall
        (for [[i tile] (map-indexed vector tiles)]
          (-> (cond
                tile [tile-face tile]

                (resolve-group-str context group)
                [tile-face (resolve-group-str context group) {:muted? true}]

                :else
                (let [[_ val suit] group
                      color (case suit "a" "#d18e29" "b" "#6fc7b3" "c" "#6a52a2" "black")]
                  [:div.tile.hand-tile {:style {:opacity 0.4
                                                :border "2px dashed #aaa"
                                                :color color}}
                   [group-glyph val color]]))
              (with-meta {:key i}))))]))])