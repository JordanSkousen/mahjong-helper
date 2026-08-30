(ns mahjong-helper.helper.group-glyph
  (:require [clojure.string :as string]
            [goog.string :as gstring]
            [mahjong-helper.const :refer [tile-keys WILDS1 WILDS2]]
            [re-re-frame.core :refer [subscribe]]))

(defn group-glyph
  "Renders one unit of a pattern group's abstract placeholder icon/character
   (e.g. for wild letter \"r\" the digit \"1\" in the traditional theme or
   the letter \"A\" otherwise, for \"D\" the dragon icon, etc) — used for
   the generic pattern shape, not a resolved concrete tile."
  [val color]
  (let [traditional-theme? @(subscribe [:traditional-theme?])]
    (if traditional-theme?
      (cond
        (string/includes? WILDS1 val)
        (js/String.fromCharCode (- (.charCodeAt val 0) 65))

        (string/includes? WILDS2 val)
        (js/String.fromCharCode (- (.charCodeAt val 0) 53))

        :else
        val)
      (cond
        ;; suitless icon
        (some #{val} ["F" "N" "E" "W" "S" "0"])
        [:img {:src @(subscribe [:svg-url val])
               :class (when (= val "0") "white-dragon")
               :alt val}]

        ;; suited icon (dragon)
        (= val "D")
        (let [url @(subscribe [:svg-url (->> tile-keys
                                             (filter #(and (not (:suit? %))
                                                           (= (:key %) "D")))
                                             first
                                             :icon)])]
          [:span {:style {:background-color color
                          "WebkitMask" (str "url(" url ") no-repeat center")
                          :mask (str "url(" url ") no-repeat center")
                          :display :inline-block
                          :width "0.7em"
                          :user-select :none
                          :margin-right "3px"}}
           (gstring/unescapeEntities "&nbsp;")])

        ;; WILDS1 sequential letter — abstract placeholder A/B/C/... for
        ;; whichever wild position this is (r->A, s->B, ...), used by the
        ;; generic pattern display (e.g. "1ra2sa3ta..." renders as
        ;; "A BB CCC ..."), not a resolved digit
        (string/includes? WILDS1 val)
        (js/String.fromCharCode (- (.charCodeAt val 0) 49))

        ;; WILDS2
        (string/includes? WILDS2 val)
        "#"

        :else
        val))))