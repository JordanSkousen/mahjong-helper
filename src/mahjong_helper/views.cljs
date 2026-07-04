(ns mahjong-helper.views
  (:require [clojure.string :as string]
            [mahjong-helper.handlers :as handlers]
            [mahjong-helper.subs :as subs]
            [mahjong-helper.utils :refer [suitless?]]
            [re-re-frame.core :refer [subscribe dispatch]]))

#_(def all-tiles (concat (->> (range 1 10)
                            (map #(str % "B"))
                            (repeat 4)
                            flatten)
                       (->> (range 1 10)
                            (map #(str % "C"))
                            (repeat 4)
                            flatten)
                       (->> (range 1 10)
                            (map #(str % "D"))
                            (repeat 4)
                            flatten)
                       (repeat 4 "DB")
                       (repeat 4 "DC")
                       (repeat 4 "DD")
                       (repeat 4 "N")
                       (repeat 4 "E")
                       (repeat 4 "W")
                       (repeat 4 "S")
                       (repeat 8 "J")
                       (repeat 8 "F")))

(def patterns ["32a30.42b46b"
               "(12a10.12a16a)3Da42b3Db"
               "(12a10.12a16a)3Da46b3Db"
               "3F.(12a10.12a16a)32b46c"
               "22a20.32b36b(1N.1E.1W.1S.)"
               "32a34a46a48a"
               "32a34a46b48b"
               "2F.42a24b26b48a"
               "2E.22a34a36a28a2W."
               "42a3Da48b3Db"
               "3F.22a24a36a48a"
               "(12a14a16a18a)42b1Db42c1Dc"
               "(12a14a16a18a)44b1Db44c1Dc"
               "(12a14a16a18a)46b1Db46c1Dc"
               "(12a14a16a18a)48b1Db48c1Dc"
               "3F.(12a14a16a18a)3F.42b"
               "3F.(12a14a16a18a)3F.44b"
               "3F.(12a14a16a18a)3F.46b"
               "3F.(12a14a16a18a)3F.48b"
               "2F.(12a14a16a)38a(12b14b16b)38b"
               "4ra6F.4rb"
               "4ra1Da3rb1Db4rc1Dc"
               "2F.4ra2rb4rc2Da"
               "2F.4ra2rb4rc2Db"
               "2F.4ra2rb4rc2Dc"
               "5ra4rb5rc"
               "2F.5ra2sa5ta"
               "5ra5ia4Db"
               "21a32a23a34a45a"
               "25a36a27a38a49a"
               "3F.4ra(1sa1ta1ua)4va"
               "3F.4ra(1sb1tb1ub)4va"
               "2ra2sa3rb3sb4tc"
               "3ra3sa4ta4ua"
               "3ra3sa4tb4ub"
               "3F.2ra2sa3ta4Da"
               "3F.2ra2sb3ta4Db"
               "4ra6F.4sa"
               "2F.4ra4sa4ta"
               "2F.4ra4sb4tc"
               "1ra2sa3ta1rb2sb3tb2uc"
               "21a33a25a37a49a"
               "21a33a25b37b49c"
               "31a33a43b45b"
               "35a37a47b49b"
               "2N.41a23a45a2S."
               "2N.45a27a49a2S."
               "2Da(13a15a17a19a)4Db4Dc"
               "3F.21a23a35a4Da"
               "3F.25a27a39a4Da"
               "21a23a31b33b45c"
               "25a27a35b37b49c"
               "41a23a25a27a49a"
               "41a23b25b27b49a"
               "2F.21a23a25a31b31c"
               "2F.25a27a29a35b35c"
               "2F.(11a13a15a)37a39a3Db"
               "4N.3E.3W.4S."
               "3N.4E.4W.3S."
               "(1ra1sa1ta1ua)3Da3Db4Dc3"
               "3N.4Da4Db3S."
               "3E.42a42b3W."
               "3E.44a44b3W."
               "3E.46a46b3W."
               "3E.48a48b3W."
               "3F.4N.3F.4Da"
               "3F.4E.3F.4Da"
               "3F.4W.3F.4Da"
               "3F.4S.3F.4Da"
               "11a1N.12a2E.13a3W.14a4S."
               "2F.4N.4S.2Da2Db"
               "2F.4E.4W.2Da2Db"
               "2N.3E.(12a10.12a16a)3W.2S."
               "33a36a46b49b"
               "33a36a46b49c"
               "23a26a33b36b49c"
               "3F.23a36a29a4Da"
               "3F.23a36a29a4Db"
               "23a26a36b39b(1N.1E.1W.1S.)"
               "2F.(23a16a19a)43b43c"
               "2F.(26a16a19a)46b46c"
               "2F.(29a16a19a)49b49c"
               "2F.33a36a39a(13b16b19b)"
               "2N.2E.2W.2S.(1ra1Da)(1rb1Db)(1rc1Dc)"
               "12a14a26a28a12b14b26b28b28c"
               "2F.(23a16a19a)(13b26b19b)(13c16c29c)"
               "2ra2sa2ta2ua2va2wa2xa"
               "21a(13a15a17a)29a21b(13b15b17b)29b"
               "2F.(12a10.12a16a)(12b10.12b16b)(12c10.12c16c)"])
(def closed-pattern? #{"2F.(12a14a16a)38a(12b14b16b)38b"
                       "1ra2sa3ta1rb2sb3tb2uc"
                       "2F.21a23a25a31b31c"
                       "2F.25a27a29a35b35c"
                       "2F.(11a13a15a)37a39a3Db"
                       "2N.3E.(12a10.12a16a)3W.2S."
                       "2F.33a36a39a(13b16b19b)"
                       "2N.2E.2W.2S.(1ra1Da)(1rb1Db)(1rc1Dc)"
                       "12a14a26a28a12b14b26b28b28c"
                       "2F.(23a16a19a)(13b26b19b)(13c16c29c)"
                       "2ra2sa2ta2ua2va2wa2xa"
                       "21a(13a15a17a)29a21b(13b15b17b)29b"
                       "2F.(12a10.12a16a)(12b10.12b16b)(12c10.12c16c)"})
                     

(defn number?*
  [s]
  (not (js/Number.isNaN (int s))))

(def WILDS1 "rstuvwxyz")
(def WILDS2 "ijklmnopq")
(def ALL_WILDS (str WILDS1 WILDS2))
(defn dragon?
  [val]
  (= val "D"))
(defn joker?
  [val]
  (= val "J"))
(defn pv-wild-num?
  [pgroup-val]
  (some #{pgroup-val} (seq WILDS1)))
(defn pv-wild-num2?
  [pgroup-val]
  (some #{pgroup-val} (seq WILDS2)))
(defn pv-wild-map
  ([pgroup-val tile-val]
   (pv-wild-map pgroup-val tile-val false))
  ([pgroup-val tile-val _2?]
   (let [arr (if _2? WILDS2 WILDS1) 
         p-idx (string/index-of arr pgroup-val)
         tile-val' (int tile-val)
         start (- tile-val' p-idx)]
     (zipmap arr
             (map str (range start (+ start (count arr) 1)))))))
(defn pattern-groups
  [pattern]
  (->> pattern
       (partition 3)
       (map (partial apply str))))

(defn tile-matches-pattern-group
  [context pattern-group tile]
  (let [{:keys [pattern]} context
        [pgroup-mul pgroup-val pgroup-suit] pattern-group
        [tile-val tile-suit] tile
        val-match (cond
                    ;; tile is joker
                    (and (joker? tile-val)
                         (> (int pgroup-mul) 2))
                    {:result true}
                    ;; tile is number, pattern is r-z val, and none picked in context yet
                    (and (number?* tile-val)
                         (pv-wild-num? pgroup-val)
                         (nil? (get context pgroup-val)))
                    {:result true
                     :context (merge context (pv-wild-map pgroup-val tile-val))}
                    ;; tile is number, pattern is r-z val, and matches one picked in context
                    (and (number?* tile-val)
                         (pv-wild-num? pgroup-val)
                         (= (get context pgroup-val) tile-val))
                    {:result true}
                    ;; tile is number, pattern is i-q val, and none picked in context yet
                    (and (number?* tile-val)
                         (pv-wild-num2? pgroup-val)
                         (nil? (get context pgroup-val)))
                    {:result true
                     :context (merge context (pv-wild-map pgroup-val tile-val true))}
                    ;; tile is number, pattern is i-q val, and matches one picked in context
                    (and (number?* tile-val)
                         (pv-wild-num2? pgroup-val)
                         (= (get context pgroup-val) tile-val))
                    {:result true}
                    ;; pattern 0 = white dragon (soap)
                    (and (= pgroup-val "0")
                         (dragon? tile-val)
                         (= tile-suit "D"))
                    {:result true}
                    ;; tile is specific number
                    (= tile-val pgroup-val)
                    {:result true})
        val-match' (update val-match :result (fn [result]
                                               (cond 
                                                 ;; generated wild map has negative val, invalid result
                                                 (some #(<= (int %) 0) (vals (select-keys (:context val-match) ALL_WILDS)))
                                                 false
                                                 ;; generated wild map has a val > 9 that will be used in pattern, invalid result
                                                 (some #(and (> (int (get-in val-match [:context %])) 9)
                                                             (string/includes? pattern %)) ALL_WILDS)
                                                 false

                                                 :else
                                                 result)))
        suit-match (cond
                     ;; suit not applicable — no constraint, nothing to bind
                     (= pgroup-suit ".")
                     {:result true}
                     ;; joker takes any suit without binding it
                     (joker? tile-val)
                     {:result true}
                     ;; suit not picked in context yet
                     (nil? (get context pgroup-suit))
                     {:result true
                      :context (assoc context pgroup-suit tile-suit)}
                     ;; suit picked in context matches
                     (= (get context pgroup-suit) tile-suit)
                     {:result true})
        result (boolean (and (:result val-match') (:result suit-match)))] 
    {:result result
     :context (cond-> context
                result (merge (:context val-match') (:context suit-match)))}))

(defn pattern-slots
  "Expand a pattern into one entry per tile it requires. Each entry keeps
   the original 3-char group — the multiplier drives the joker rule."
  [pattern]
  (->> (string/replace pattern #"[()]" "")
       pattern-groups
       (mapcat (fn [group]
                 (let [[mul] group]
                   (repeat (int mul) group))))
       vec))

(defn- vec-remove
  [v i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn rank-pattern
  "Max number of tiles from hand that can fill pattern's slots under one
   consistent context (wild numbers + suit letters). Backtracking with
   branch & bound. Within a run of identical slots, tiles are taken in
   sorted order and a skip jumps the whole run, so interchangeable
   assignments aren't re-explored."
  [pattern hand]
  (let [slots (pattern-slots pattern)
        n-slots (count slots)
        best (atom 0)]
    (letfn [(dfs [slot-i tiles min-i context matched]
              (swap! best max matched)
              (when (and (< slot-i n-slots)
                         (seq tiles)
                         (> (+ matched (min (- n-slots slot-i) (count tiles)))
                            @best))
                (let [slot (slots slot-i)
                      same-next? (and (< (inc slot-i) n-slots)
                                      (= slot (slots (inc slot-i))))]
                  (doseq [i (range min-i (count tiles))
                          ;; only the first of a run of duplicate tiles
                          :when (or (= i min-i)
                                    (not= (tiles i) (tiles (dec i))))]
                    (let [{:keys [result context]}
                          (tile-matches-pattern-group context slot (tiles i))]
                      (when result
                        (dfs (inc slot-i)
                             (vec-remove tiles i)
                             (if same-next? i 0)
                             context
                             (inc matched)))))
                  ;; leave this run of identical slots unfilled
                  (let [run-end (loop [j (inc slot-i)]
                                  (if (and (< j n-slots) (= slot (slots j)))
                                    (recur (inc j))
                                    j))]
                    (dfs run-end tiles 0 context matched)))))]
      (dfs 0 (vec (sort hand)) 0 {:pattern pattern} 0)
      @best)))

(defn rank-patterns
  "hand is a vec of tile strings like [\"5B\" \"DC\" \"N\" \"J\" \"F\"].
   Returns {pattern ranking}; a higher ranking means the hand is closer
   to that mahjong (ranking = tiles already in place)."
  [hand]
  (into {}
        (map (fn [pattern] [pattern (rank-pattern pattern hand)]))
        patterns))

(def suits {"B" {:color "#4cb970"
                 :name "Bamb"
                 :icon (fn [color]
                         [:svg#Layer_1 {:data-name "Layer 1"
                                        :xmlns "http://www.w3.org/2000/svg"
                                        :viewBox "0 0 36.85 36.85"}
                          [:rect {:fill color :x "17.9" :y "4.56" :width "4.55" :height "27.78"}]
                          [:rect {:fill color :x "11.15"
                                        :y "11.26"
                                        :width "4.55"
                                        :height "17.05"
                                        :transform "translate(-10.06 15.29) rotate(-45)"}]
                          [:rect {:fill color :x "10.33" :y "8.89" :width "4.55" :height "4.55"}]
                          [:rect {:fill color :x "3.68" :y "17.8" :width "4.55" :height "4.55"}]
                          [:rect {:fill color :x "28.62" :y "12.15" :width "4.55" :height "4.55"}]
                          [:rect {:fill color :x "23.26"
                                        :y "3.62"
                                        :width "4.55"
                                        :height "17.05"
                                        :transform "translate(16.07 -14.5) rotate(45)"}]])}
            "C" {:color "#d9315d"
                 :name "Crak"
                 :icon (fn [color]
                         [:svg#Layer_1 {:data-name "Layer 1"
                                        :xmlns "http://www.w3.org/2000/svg"
                                        :viewBox "0 0 36.85 36.85"}
                          [:path
                           {:fill color :d
                            "M27.95,30.43h-2.68c-1.93,0-2.96-.68-3.31-2.3l-.89.12c-4.55.61-9.41,1.19-12.58,1.55-.22.23-.51.38-.83.43l-.89.15-1.42-4.15,1.35-.12c.91-.08,1.97-.18,3.13-.3.64-4.33,1.37-10.54,1.81-15.56h-5.45v-3.82h24.72v3.82h-15.37l-.04.36c-.05.56-.11,1.14-.16,1.72h10.36v14.33h1.85c.16-.71.21-2.64.23-3.73l.06-2.26,1.71,1.49c.22.19.73.43,1.13.54l.82.22-.03.85c-.12,3.43-.24,6.67-3.52,6.67ZM14.57,19.58c-.24,2.05-.48,4.07-.71,5.8,1.71-.18,3.5-.38,5.29-.59l-.35-.63c-.71-1.28-2.45-3.15-4.23-4.58ZM21.83,23.37l-1.71,1.3c.22-.03.43-.05.65-.08l1.05-.13v-1.1ZM16.15,16.14l.63.46c1.74,1.26,3.91,3.27,5.05,5.06v-5.54h-6.87c-.04.32-.07.64-.11.97l1.3-.95Z"}]])}
            "D" {:color "#3682c4"
                 :name "Dot"
                 :icon (fn [color]
                         [:svg#Layer_1 {:data-name "Layer 1"
                                        :xmlns "http://www.w3.org/2000/svg"
                                        :viewBox "0 0 36.85 36.85"}
                          [:circle {:fill color :cx "18.43" :cy "18.43" :r "12.65"}]])}})
(def tile-keys ["1"
                "2"
                "3"
                {:key "B" :suit? true :icon "Bamb"}
                "4"
                "5"
                "6"
                {:key "C" :suit? true :icon "Crak"}
                "7"
                "8"
                "9"
                {:key "D" :suit? true :icon "Dot"}
                {:key "N" :icon "N"}
                {:key "E" :icon "E"}
                {:key "W" :icon "W"}
                {:key "S" :icon "S"}
                {:key "Dragon" :icon (fn [style]
                                       [:svg#Layer_1 {:data-name "Layer 1"
                                                      :xmlns "http://www.w3.org/2000/svg"
                                                      :viewBox "0 0 36.85 51.02"}
                                        [:path
                                         (merge style {:d "M25.72,36.77c1.69.55,3.03,1.53,4.47,2.63.14-5.92-5.48-9.61-10.46-11.73-3.03-1.29-5.04-3.62-4.48-6.67.25-1.34,1.35-2.73,2.8-3.51,0,3.8,2.26,5.34,6,5.65,2.78.23,5.34,1.37,4.84,2.15l-.8,1.24c1.47.25,2.93.41,4.38.29s2.66-1.35,2.37-2.88c-.31-1.66-1.18-3.26-2.05-4.6-.41.69-.67,1.2-.94,1.43s-1.21-.02-1.49-.26c-2.24-1.92-.9-4.23-2.69-5.47-3.83-2.66-5.04-7.68-8.66-11.32l1.05,4.71c-2.39-1.82-4.39-3.43-7.11-4.86,1.98,2.8,4.12,5,5.62,8.15-3.82-2.12-7.74-3.5-11.47-.76,1.03.06,2.04.09,2.72.68.3.26.42,1.27.25,1.61-.81,1.6-2.96,1.34-5.38,2.69-1.45.81-2.62,2.11-2.72,3.86,1.04-.43,2-.88,3.02-.63.31.08.79.83.64,1.11l-.66,1.18c-2.58,1.56-3.41,4.6-2.07,7.23.18-1.14.88-2.32,2.01-1.97,1.18.37,1.64,1.73,2.11,2.79,2.87,6.42,11.82,7.42,13.29,13.07.36,1.38-.13,2.77-1.34,3.52s-2.57.91-4.08,1.34c3.51.58,7.13-.33,9.59-2.72,2.21-2.14,2.52-4.96,1.22-7.96ZM25.54,17.87l-1.99-2.83,4.11,2.42-2.12.42ZM29.46,22.58l.33-.88c.14-.38,1.7-.15,2.23,1.68-.99-.62-1.28-1.48-2.56-.8ZM10.42,40.73c.68-.08,1.63-.57,1.81-1.07.21-.6.07-1.45-.07-2.23,1.83.86,3.49,2.26,4.37,4.07s.11,3.7-1.8,4.34c-5.68,1.9-11.77-2.43-12.77-8.36,2.47,2.36,5.29,3.64,8.46,3.25Z"})]])}
                {:key "Flower" :icon "F"}
                {:key "J" :icon "J"}
                {:key [:span.material-symbols-outlined {:style {:font-size "1em"}} "backspace"]
                 :on-click #(dispatch [::handlers/backspace])
                 :style {:background "black"
                         :color "white"}}])

(def rank-patterns-memo (memoize rank-patterns))

(defn hand-tile
  "A completed tile in the hand; tap to edit it."
  [idx tile]
  (let [{:keys [value suit]} tile
        needs-suit? (not (suitless? value))
        editing? (= idx @(subscribe [::subs/editing-idx]))]
    [:button.tile.hand-tile {:on-click #(do (dispatch [::handlers/edit-tile idx])
                                            (js/window.scrollTo #js{:top 0 :behavior "smooth"}))
                             :class [(when (and (not editing?) 
                                               (or (not value) 
                                                   (and needs-suit? (not suit)))) 
                                      "pending-tile")
                                     (when editing?
                                       "editing-tile")]
                             :style {:border-color (if editing? "#f1b81a" "#ccc")
                                     :background (when-not editing?
                                                   (cond
                                                     suit (get-in suits [suit :color])
                                                     :else "#fff"))
                                     :color (if (and suit (not editing?)) "white" "black")}}
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
     (when suit
       [(get-in suits [suit :icon]) (if (and suit (not editing?)) "#fff" "#000")])]))

(defn hand-view []
  (let [hand @(subscribe [::subs/hand])]
    [:div#hand-view
     [:div {:style {:display "flex"
                    :justify-content "space-between"
                    :align-items "baseline"
                    :margin-bottom "6px"}}
      [:span "Your Hand"]
      [:span {:style {:color "#888" :font-size "13px"}}
       (str @(subscribe [::subs/num-completed-tiles]) " / 13")]]
     [:div {:style {:display "flex"
                    :flex-wrap "wrap"
                    :gap "6px"
                    :min-height "58px"}} 
      (doall
       (for [idx (range 13)]
         (let [tile (get hand idx)]
           ^{:key idx} [hand-tile idx tile])))]]))

(defn keyboard []
  (let [editing-idx @(subscribe [::subs/editing-idx])]
    [:div {:style {:display "grid"
                   :grid-template-columns "repeat(4, 1fr)"
                   :gap "6px"}}
     (doall
      (for [item tile-keys]
        (let [{:keys [key icon on-click suit? style]} (if (string? item)
                                                        {:key item}
                                                        item)]
          [:button.tile {:key key
                         :on-click (or on-click #(dispatch [(if suit? ::handlers/key-suit ::handlers/key-value) key]))
                         :style (if suit?
                                  {:background (get-in suits [key :color])}
                                  style)}
           (cond 
             (fn? icon)
             [icon]
             icon
             [:img {:src (str "/img/" icon ".svg")}]
             :else
             key)])))
     [:button.tile.tile-sm {:on-click #(dispatch [::handlers/edit-tile (if (= editing-idx 0)
                                                                         12
                                                                         (dec editing-idx))])
                            :style {:background "black"
                                    :color "white"
                                    :grid-column "1 / 3"}}
      [:span.material-symbols-outlined "chevron_backward"]]
     [:button.tile.tile-sm {:on-click #(dispatch [::handlers/edit-tile (if (= editing-idx 12)
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

(defn results-view []
  (let [hand @(subscribe [::subs/hand])]
    (when (>= @(subscribe [::subs/num-completed-tiles]) 13)
      [:<>
       [:hr {:style {:margin-top "20px"
                     :margin-left "-20px"
                     :margin-right "-20px"}}]
       [:div {:style {:margin-top "16px"}} 
        [:div {:style {:margin-bottom "6px"}} "Closest Mahjongs"]
        (for [[pattern ranking] (->> (rank-patterns-memo (->> hand
                                                              vals
                                                              (map #(str (:value %) (get % :suit ".")))))
                                     (sort-by val >))]
          ^{:key pattern}
          [:div.pattern {:style {:display "flex"
                                 :align-items "center"
                                 :gap "10px"
                                 :padding "6px 0"
                                 :border-bottom "1px solid #eee"}}
           [:span.ranking (/ (js/Math.floor (* (/ ranking 14) 1000)) 10) "%"]
           [display-pattern pattern]])]])))

(defn Main []
  [:<>
   [:div {:style {:max-width "480px"
                  :margin "0 auto"
                  :padding "12px 12px 36px"}}
    [:div.title
     [:div {:style {:padding "10px"}}
      "MAHJONG HELPER"
      [:a {:href "/2026-Mahjong-Cheatsheet-by-Jordan-Skousen.pdf"
           :target "_blank"
           :rel "noreferrer"} [:span.material-symbols-outlined "print"]]]]
    [:div {:style {:height "calc(1.5em + 20px)"}}]
    [hand-view]
    [results-view]
    [:div {:style {:visibility :hidden}}
     (when (>= @(subscribe [::subs/editing-idx]) 0)
       [keyboard])]]
   (when (>= @(subscribe [::subs/editing-idx]) 0)
     [:div.keyboard
      [keyboard]])])