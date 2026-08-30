(ns mahjong-helper.helper.display-pattern
  (:require [clojure.string :as string]
            [mahjong-helper.const :refer [patterns WILDS1 WILDS2]]
            [mahjong-helper.helper.group-glyph :refer [group-glyph]]
            [mahjong-helper.helper.solver :refer [pattern-groups]]
            [re-re-frame.core :refer [subscribe]]))

(defn display-group
  [s]
  (let [[mul val suit] s
        traditional-theme? @(subscribe [:traditional-theme?])
        color (if traditional-theme?
                (case suit
                  "a" "green"
                  "b" "red"
                  "black")
                (case suit
                  "a" "#d18e29"
                  "b" "#6fc7b3"
                  "c" "#6a52a2"
                  "black"))]
    [:span.group {:style {:color color}}
     (doall
      (for [idx (range mul)]
        ^{:key idx}
        [group-glyph val color]))]))

(defn display-pattern
  [pattern inline?]
  (let [split (string/split pattern #"\(|\)")
        traditional-theme? @(subscribe [:traditional-theme?])
        num-suits-in-pattern (->> (string/replace pattern #"[()]" "")
                                  pattern-groups
                                  (map last)
                                  (filter #(not= % "."))
                                  (apply hash-set)
                                  count)]
    [:div {:style {:display (if inline? :inline :block)}}
     (->> split
          (map-indexed (fn [idx s]
                         (when-not (string/blank? s)
                           ^{:key idx}
                           [:span (->> s
                                       pattern-groups
                                       (map-indexed (fn [idx2 group]
                                                      ^{:key (str idx "-" idx2)}
                                                      [:span (display-group group) (when (even? idx) " ")]))
                                       doall) " "])))
          doall)
     (when traditional-theme?
       [:span {:style {:font-size "0.6em"
                       :color "#666"
                       :margin-right "10px"}}
        "("
        (cond
          (some #(string/includes? pattern %) WILDS2) "Any 2 Nos."
          (->> WILDS1
               (filter #(string/includes? pattern %))
               count
               (= 1)) "Any Like No."
          (some #(string/includes? pattern %) WILDS1) "Any Consec. Nos."
          :else "These Nos. Only")
        (when (> num-suits-in-pattern 0)
          (str ", Any " num-suits-in-pattern " Suit" (when (not= num-suits-in-pattern 1) "s")))
        ")"])
     (when (get-in patterns [pattern :closed?])
       [:img.closed {:src @(subscribe [:svg-url "Closed"])
                     :alt "CLOSED"}])]))