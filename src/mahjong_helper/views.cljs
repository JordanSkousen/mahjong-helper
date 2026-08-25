(ns mahjong-helper.views
  (:require ["@dnd-kit/react" :refer [DragDropProvider useDraggable useDroppable]]
            [clojure.string :as string]
            [goog.string :as gstring]
            [mahjong-helper.const :refer [patterns suits tile-keys WILDS1 WILDS2]]
            [mahjong-helper.modal :refer [Modal]]
            [mahjong-helper.solver :refer [pattern-groups find-arrangements
                                           groups-with-slots resolve-group-str]]
            [mahjong-helper.utils :refer [read-storage suitless? dragon?]]
            [mahjong-helper.views.menu-btn :refer [Menu-Btn]]
            [mahjong-helper.worker-pool :refer [rank-patterns-async]]
            [re-re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as r]))

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

(defn draggable-hand-tile
  [{:keys [idx tile group-id id disabled?]}]
  (let [ref (.-ref (useDraggable #js {:id (if group-id 
                                            (str group-id "/" id)
                                            idx)}))]
    [:div.tile.hand-tile {:ref ref
                          :style (merge (hand-tile-style idx tile type)
                                        (when disabled?
                                          {:opacity 0.25
                                           :pointer-events :none}))}
     [hand-tile-inner idx tile :meld]]))

(defn hand-tile
  "A completed tile in the hand; tap to edit it."
  [idx tile type]
  (let [{:keys [value suit melded?]} tile
        type (or type :normal)
        normal? (= type :normal)
        charleston? (= type :charleston)
        needs-suit? (not (suitless? value))
        editing? (and normal?
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

(defn group-glyph
  "Renders one unit of a pattern group's placeholder icon/character (e.g.
   for wild letter \"r\" the digit \"1\", for \"D\" the dragon icon, etc)."
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

        ;; WILDS1 sequential letter
        (string/includes? WILDS1 val)
        (js/String.fromCharCode (- (.charCodeAt val 0) 49))

        ;; WILDS2
        (string/includes? WILDS2 val)
        "#"

        :else
        val))))

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


(defn tile-face
  "Renders a concrete 2-char tile string (\"5B\", \"N.\", \"F.\", \"DB\",
   \"DD\" for soap...) the way hand-tile does. `muted?` shows it as a
   still-needed tile rather than one already in hand."
  [tile-str & [{:keys [muted?]}]]
  (let [value (subs tile-str 0 1)
        suit (let [s (subs tile-str 1 2)] (when (not= s ".") s))
        soap? (and (dragon? value) (= suit "D"))
        {:keys [icon]} (->> tile-keys
                            (filter #(and (= (:key %) value)
                                          (not (:suit? %))))
                            first)]
    [:div.tile.hand-tile {:class (when muted? "tile-muted")
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

(defn result-modal []
  (let [pattern @(subscribe [:result-modal-open-pattern])
        hand-as-strs @(subscribe [:hand-as-strs])
        {:keys [id category]} (get patterns pattern)
        arrangements (when pattern (find-arrangements pattern hand-as-strs))]
    [Modal {:open? (some? pattern)
            :title [:div {:style {:font-weight :normal}}
                    [:div.result-modal-top
                     [:span.chip category]
                     (when-not @(subscribe [:traditional-theme?])
                       [:code {:style {:color :gray
                                       :font-weight :normal}}
                        "#" id " "])]
                    [:div.result-modal-pattern
                     [display-pattern pattern true]]]
            :closable? true
            :on-close #(dispatch [:open-result-modal nil])}
     [:div.result-modal
      (when pattern
        (if (<= (count arrangements) 1)
          [arrangement-view pattern (first arrangements)]
          (doall
           (for [[i arr] (map-indexed vector arrangements)]
             ^{:key i}
             [:div
              [:div {:style {:font-weight 700
                             :margin-top (when (pos? i) "16px")
                             :color "#666"
                             :font-size "0.7em"}}
               (str "Option " (inc i))]
              [arrangement-view pattern arr]]))))
      [:button {:style {:position :sticky
                        :bottom 0}
                :on-click #(dispatch [:open-result-modal nil])}
       "Close"]]]))

;; Ranking every pattern against a full hand is a somewhat expensive
;; backtracking search (~130 patterns), so it's farmed out to a pool of
;; Web Workers (see worker-pool.cljs) instead of blocking the render.
;; :hand tracks which hand the current :rankings were computed for, so
;; a stale in-flight computation can be told apart from a fresh one.
(defonce ^:private pattern-rankings (r/atom nil))
(defonce ^:private rankings-computed-for (atom nil))

(defn- ensure-rankings!
  [hand-as-strs]
  (when (not= @rankings-computed-for hand-as-strs)
    (reset! rankings-computed-for hand-as-strs)
    (rank-patterns-async hand-as-strs
                         (fn [results]
                           ;; drop the result if a newer hand has since superseded it
                           (when (= @rankings-computed-for hand-as-strs)
                             (reset! pattern-rankings {:hand hand-as-strs :rankings results}))))))

(defn results-view []
  (let [hand-as-strs @(subscribe [:hand-as-strs])
        preview-mode? @(subscribe [:preview-mode?])
        traditional-theme? @(subscribe [:traditional-theme?])]
    (when @(subscribe [:hand-complete?])
      (ensure-rankings! hand-as-strs)
      (let [{:keys [hand rankings]} @pattern-rankings
            computing? (not= hand hand-as-strs)]
        [:<>
         [:hr {:style {:margin-top "20px"
                       :margin-left "-20px"
                       :margin-right "-20px"}}]
         [:div {:style {:margin-top "16px"}}
          [:div {:style {:margin-bottom "6px"}}
           [:span "Closest Mahjongs"]
           (when computing?
             [:span {:style {:margin-left "8px" :color "#888" :font-size "0.7em"}}
              "Calculating…"])
           [:span {:style {:float :right}
                   :on-click #(dispatch [:toggle-preview-mode])}
            [:div.switch {:class (when preview-mode? "switch-on")}
             [:div.switch-knob]]
            " Preview Mode"]]
          [:div {:style {:opacity (when computing? 0.5)}}
           (doall
            (for [{:keys [id pattern ranking arrangement]}
                  (->> rankings
                       (map (fn [[pattern {:keys [ranking arrangement]}]]
                              {:id (get-in patterns [pattern :id])
                               :pattern pattern
                               :ranking ranking
                               :arrangement arrangement}))
                       (sort-by :id)
                       (sort-by :ranking >))
                  :let [ranking-pct (/ ranking 14) ;; this should always be 14 since you need 14 tiles to get mahjong, duh
                        ]] 
              ^{:key pattern}
              [:div.pattern {:on-click #(dispatch [:open-result-modal pattern])}
               (when-not traditional-theme?
                 [:code.pattern-id "#" id])
               [:div.pattern-container
                (if preview-mode?
                  ;; precomputed in the worker alongside the ranking, so
                  ;; toggling Preview Mode doesn't force a synchronous
                  ;; find-arrangements pass over every pattern
                  [arrangement-view pattern arrangement]
                  [display-pattern pattern])
                [:div.pattern-percent
                 [:div {:style {:width (str (max 1 (* ranking-pct 100)) "%")
                                :background (str "hsl(" (* ranking-pct 120) ", 73%, 41%)")}}]
                 [:span (/ (js/Math.floor (* ranking-pct 1000)) 10) "%"]]]
               [:span.pattern-view {:style {:color "rgb(0, 166, 255)"}}
                [:span.material-symbols-outlined "visibility"]]]))]]]))))

(defn settings []
  (let [theme @(subscribe [:theme])
        starting-player? @(subscribe [:starting-player?])]
    [:div {:style {:font-size "1.2em"}}
     [:div {:style {:display :flex
                    :align-items :centfr}}
      "Are you starting player?"
      [:div.button-group {:style {:margin-left 10 
                                  :display :inline-flex
                                  :flex "1 0"
                                  }}
       [:button.btn-sm {:class (when starting-player? "button-active") 
                        :on-click #(dispatch [:starting-player true])}
        "Yes"]
       [:button.btn-sm {:class (when-not starting-player? "button-active")
                        :on-click #(dispatch [:starting-player false])}
        "No"]]]
     [:div {:style {:margin-top 10}}
      "Theme: "
      [:div.button-group {:style {:display :inline-flex
                                  :width :fit-content}}
       (doall
        (for [[key label] {:jordan "Jordan's"
                           :traditional "Traditional"}]
          [:button.btn-sm {:class (when (= theme key) "button-active")
                           :on-click #(dispatch [:theme key])}
           label]))]]]))

(defn starting-modal [] 
  [Modal {:open? @(subscribe [:starting-modal-open?])
          :title "Settings"}
   [settings]
   [:hr]
   [:button {:style {:background "rgb(10, 159, 17)"
                     :color "white"}
             :on-click #(dispatch [:close-starting-modal])}
    "Start"]])

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
           ^{:key idx} [hand-tile idx tile :charleston])))]
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

(defn meld-drop-zone
  [group-id id]
  (let [ref (.-ref (useDroppable #js {:id (str group-id "/" id)}))]
    [:div.tile.hand-tile.pending-tile {:ref ref}]))

(defn meld-modal []
  (let [hand @(subscribe [:hand])
        show-invalid? @(subscribe [:meld-modal-show-invalid?])
        meld-modal-groups @(subscribe [:meld-modal-groups])
        meld-modal-groups-flat (->> meld-modal-groups
                                    vals
                                    (mapcat vals)
                                    (apply hash-set))]
    [Modal {:open? @(subscribe [:meld-modal-open?])
            :title "Choose all tiles you've melded"
            :closable? true
            :on-close #(dispatch [:close-meld-modal])}
     [:> DragDropProvider {:on-drag-end (fn [evt]
                                          (when-not (.-canceled evt) 
                                            (if-let [target (.. evt -operation -target)]
                                              (let [tile-idx (int (.. evt -operation -source -id))
                                                    [group-id id] (string/split (.-id target) #"/")]
                                                (dispatch [:meld-add-to-group (uuid group-id) (int id) tile-idx]))
                                              (let [[source-group-id source-id] (string/split (.. evt -operation -source -id) #"/")]
                                                (dispatch [:meld-remove-from-group (uuid source-group-id) (int source-id)])))))}
      [:div {:style {:display "flex"
                     :flex-wrap "wrap"
                     :gap "6px"
                     :min-height "58px"}}
       (doall
        (for [idx (range @(subscribe [:hand-size]))]
          (let [tile (get hand idx)]
            ^{:key idx} [:f> draggable-hand-tile {:idx idx 
                                                  :tile tile
                                                  :disabled? (some #{idx} meld-modal-groups-flat)}])))]
      [:hr]
      [:div {:style {:display "flex"
                     :flex-wrap "wrap"
                     :gap "6px"
                     :min-height "58px"}}
       (doall
        (for [[group-id zones] meld-modal-groups]
          ^{:key group-id} 
          [:div.meld-group
           (doall
            (for [[id tile-idx] zones]
              (if tile-idx
                [:f> draggable-hand-tile {:idx tile-idx
                                          :tile (get hand tile-idx)
                                          :group-id group-id
                                          :id id}]
                [:f> meld-drop-zone group-id id])))]))]
      (when show-invalid?
        [:div {:style {:margin "10px 0"
                       :color :red}}
         "The tiles you've melded are not valid according to NMJL rules! (Press 'Save' again to ignore.)"])
      [:div.buttons-row
       [:button {:style {:background "red"
                         :color "white"
                         :border :none}
                 :on-click #(dispatch [:close-meld-modal])}
        "Cancel"]
       [:button {:style {:background "green"
                         :color "white"
                         :border :none}
                 :on-click #(dispatch [:save-meld])}
        "Save"]]]]))

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
    [:button {:on-click #(dispatch [:initialize-db (read-storage true)])}
     "Yes, Reset"]]])

(defn settings-modal []
  [Modal {:open? @(subscribe [:settings-modal-open?])
          :title "Settings"
          :closable? true
          :on-close #(dispatch [:close-settings-modal])}
   [settings] 
   [:div {:style {:font-size "0.1em"
                  :text-align :center
                  :color "gray"
                  :margin-top 20}}
    "2026-08-09"]
   [:button {:on-click #(dispatch [:close-settings-modal])}
    "Close"]])

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
        #_[meld-modal]
        [reset-modal]
        [settings-modal]
        [result-modal]
        [:div.title
         [:div {:style {:padding "10px"}}
          [Menu-Btn]
          "MAHJONG HELPER"
          [:div.title-right
           [:button {:style {:font-size "0.8rem"
                             :background "rgb(243, 142, 26)"}
                     :on-click #(dispatch [:open-settings-modal])}
            "Settings"]
           [:button.clear-btn {:on-click #(dispatch [:open-reset-modal])}
            "Reset"]]]]
        [:div {:style {:height "calc(1.5em + 20px)"}}]
        [hand-view]
        (when @(subscribe [:hand-complete?])
          [:<>
           [:div.charleston
            [:button.arrow-btn {:on-click #(dispatch [:open-charleston-modal])}
             "Charleston"]]
           
           [:div.meld
            [:button.arrow-btn {:on-click #(dispatch [:open-meld-modal])}
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