(ns mahjong-helper.solitaire.views 
  (:require ["@mui/material/Divider" :default Divider]
            ["@mui/material/Stack" :default Stack]
            ["@mui/material/Slider" :default Slider]
            ["@mui/material/styles" :refer [styled]]
            [mahjong-helper.views.menu-btn :refer [Menu-Btn]]
            [re-re-frame.core :refer [reg-event-x reg-grab grab subscribe dispatch]]
            [reagent.core :as r]))

(defn deep-merge
  "Merges two maps deeply, as opposed to the shallow merge built-in `merge` fn does."
  [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs)
      (reduce #(rec-merge %1 %2) v vs)
      (last vs))))

(defn layer-map->coord-pairs
  [layer]
  (->> layer
       (mapcat (fn [[x ys]]
                 (->> ys
                      keys
                      (map (fn [y]
                             [x y])))))))

(defn coord-pairs->layer-map
  [coords]
  (let [xs (->> coords
                (map first)
                (apply hash-set))]
    (zipmap xs
            (->> xs
                 (map (fn [x]
                        (zipmap (->> coords
                                     (filter #(= (first %) x))
                                     (map last))
                                (repeat true))))))))

(defn symmetricalize
  [layer symmetry]
  (->> (range symmetry)
       (map (fn [idx]
              (let [flip #(-> % (* -1) dec)]
                (case idx
                  0 layer
                  1 (update-keys layer flip) ;; convert quadrant I to quadrant II 
                  2 (-> layer
                        (update-keys flip)
                        (update-vals (fn [ys]
                                       (update-keys ys flip)))) ;; convert quadrant I to quadrant III
                  3 (update-vals layer (fn [ys]
                                         (update-keys ys flip))) ;; convert quadrant I to quadrant IV
                  ))))
       (apply deep-merge)))

(defn desymmetricalize
  [layer]
  (->> layer
       (filter (fn [[x]]
                 (>= x 0)))
       (map (fn [[x ys]]
              [x (->> ys
                      (filter (fn [[y]]
                                (>= y 0)))
                      (into {}))]))
       (into {})))

(defn adjacents-to-coord
  [[x y]]
  [[x (inc y)] ;; up
   [x (dec y)] ;; down
   [(inc x) y] ;; right
   [(dec x) y] ;; left
   ])

(defn tiles-on-edge
  "layer should be symmetricalized"
  [layer]
  (let [layer-coords (layer-map->coord-pairs layer)]
    (->> layer-coords
         (filter (fn [coord]
                   (some #(not (get-in layer %)) (adjacents-to-coord coord)) ;; if any spaces adjacent to this tile are empty, it must be on the edge
                   ))
         coord-pairs->layer-map)))

(defn generate-upper-layer
  "layer should be symmetricalized"
  [layer]
  (let [tiles-on-edge (tiles-on-edge layer)]
    (->> layer
         layer-map->coord-pairs
         (filter (fn [[x y]]
                   (not (get-in tiles-on-edge [x y]))))
         coord-pairs->layer-map)))

(defn count-tiles-in-board
  [board]
  (->> board
       vals
       (map #(-> % layer-map->coord-pairs count))
       (apply +)))

(defn remove-empty-layers 
  [board]
  (->> board
       (filter (fn [[_ layer]]
                 (seq layer)))
       (into {})))
 
(defn center-y
  ;; we must move everything so it centers around (0,0) so it renders properly
  ;; we don't care about centering x since it's always symmetrically balanced
  [board]
  (let [base-layer-ys (->> (get board 0)
                           vals
                           (mapcat keys))
        min-base-layer-y (apply min base-layer-ys)
        max-base-layer-y (apply max base-layer-ys)
        board-height (inc (- max-base-layer-y min-base-layer-y))
        shift-amt (- max-base-layer-y (/ board-height 2))]
    (->> board
         (map (fn [[layer-idx layer]]
                [layer-idx (->> layer
                                layer-map->coord-pairs
                                (map (fn [[x y]]
                                       [x (- y shift-amt)]))
                                coord-pairs->layer-map)]))
         (into {}))))

(defn generate-board
  "base-layer should NOT be symmetricalized
   
   returns symmetricalized board, structured as map of layers (0 being bottom-most) -> map of x -> map of y"
  [{:keys [symmetry max-layers base-layer num-tiles] :as args}]
  (let [base-layer' (symmetricalize base-layer symmetry)
        board (reduce (fn [board idx]
                        (assoc board idx (generate-upper-layer (get board (dec idx)))))
                      {0 base-layer'}
                      (range 1 max-layers))]
    (if (>= (count-tiles-in-board board) num-tiles)
      ;; all done!
      (->> board
           remove-empty-layers
           center-y)
      ;; add a tile adjacent to a random edge tile
      (let [edge-tile-coords (-> base-layer'
                                 tiles-on-edge
                                 desymmetricalize
                                 layer-map->coord-pairs)
            !adjacents (atom [])]
        (while (empty? @!adjacents)
          ;; no valid adjacents found (or not done yet), choose a different random edge tile
          (reset! !adjacents (->> edge-tile-coords
                                  rand-nth
                                  adjacents-to-coord
                                  (filter (fn [[x y]]
                                            (let [base-layer'' (-> base-layer
                                                                   (assoc-in [x y] true)
                                                                   (symmetricalize symmetry))
                                                  board-with-adjacent (reduce (fn [board idx]
                                                                                (assoc board idx (generate-upper-layer (get board (dec idx)))))
                                                                              {0 base-layer''}
                                                                              (range 1 max-layers))]
                                              (and (not (get-in base-layer [x y])) ;; cannot be an already existing tile
                                                   (<= (count-tiles-in-board board-with-adjacent) num-tiles) ;; if this tile would put the total # of tiles over num-tiles, skip it
                                                   (if (= symmetry 2)
                                                     ;; in 2-symmetry mode, must be in quadrants I or IV
                                                     (>= x 0)
                                                     ;; in 4-symmetry mode, must be in quadrant I
                                                     (and (>= x 0)
                                                          (>= y 0))))))))))
        (let [[rand-adjacent-x rand-adjacent-y] (rand-nth @!adjacents)]
          (-> args
              (assoc-in [:base-layer rand-adjacent-x rand-adjacent-y] true)
              generate-board))))))

(def TILE_WIDTH 30)
(def TILE_HEIGHT 40)
(def TILE_THICKNESS 10)
(def SCALE_PADDING 40)
(def BOTTOM_PADDING 80)
;; structured as (layer idx) -> [top, side]
(def COLORS {0 ["#ef9a46" "#b76b2a"]
             1 ["#7754a3" "#4e327c"]
             2 ["#64bc46" "#41853e"]
             3 ["#e42a29" "#aa1f23"]
             4 ["#3181c4" "#1f699a"]})
(def SETTINGS_PANEL_HEIGHT 110)
(defn settings-panel []
  (let [symmetry @(subscribe [::symmetry])
        num-tiles @(subscribe [::num-tiles])]
    [:div.solitaire-settings-panel {:style {:height SETTINGS_PANEL_HEIGHT}}
     [:div {:style {:display :flex
                    :align-items :center
                    :margin-bottom 10}}
      [Menu-Btn]
      [:h1 "MAHJONG HELPER - Solitaire Generator"]]
     [:> Divider {:textAlign :left}
      "SETTINGS"]
     [:> Stack {:direction :row
                :sx {:align-items :center}
                :spacing 2}
      [:> Stack {:class "solitaire-settings-symmetry"
                 :spacing 2}
       [:span "Symmetry"]
       [:div.button-group
        [:button.btn-sm {:class (when (= symmetry 2) "button-active")
                         :on-click #(dispatch [::symmetry 2])}
         2]
        [:button.btn-sm {:class (when (= symmetry 4) "button-active")
                         :on-click #(dispatch [::symmetry 4])}
         4]]]
      [:> Divider {:orientation :vertical
                   :variant :middle
                   :flexItem true}]
      [:> Stack {:class "solitaire-settings-max-layers"
                 :spacing 2}
       [:span "Max Layers"]
       [:> ((styled Slider) (fn []
                              (clj->js {"& .MuiSlider-markLabel" {:top "18px"
                                                                  :font-size "10px"}
                                        "&" {:padding-top 0}})))
        {:value @(subscribe [::max-layers])
         :onChange (fn [_ val]
                     (dispatch [::max-layers val]))
         :min 2
         :max 5
         :step 1
         :marks (->> (range 2 7)
                     (map (fn [i]
                            {:value i
                             :label i})))
         :valueLabelDisplay :auto
         :size :small}]]
      [:> Divider {:orientation :vertical
                   :variant :middle
                   :flexItem true}]
      [:> Stack {:class "solitaire-settings-mahjong-set"
                 :spacing 2}
       [:span "Mahjong Set"]
       [:div.button-group
        [:button.btn-sm {:class (when (= num-tiles 152) "button-active")
                         :on-click #(dispatch [::num-tiles 152])}
         "🇺🇸"]
        [:button.btn-sm {:class (when (= num-tiles 144) "button-active")
                         :on-click #(dispatch [::num-tiles 144])}
         "🇨🇳"]]]]]))

(defn Main []
  (r/create-class
   {:component-did-mount
    (fn []
      (letfn [(set-bounds []
                (dispatch [::set-view-bounds
                           (- (.-innerWidth js/window) (* SCALE_PADDING 2))
                           (- (.-innerHeight js/window) (* SCALE_PADDING 2) SETTINGS_PANEL_HEIGHT BOTTOM_PADDING)]))]
        (dispatch [::generate-board])
        (set-bounds)
        (js/window.addEventListener "resize" set-bounds)))
  
    :reagent-render
    (fn []
      (let [board @(subscribe [::board])
            board-width (let [base-layer-xs (keys (get board 0))]
                          (inc (- (apply max base-layer-xs) (apply min base-layer-xs))))
            base-layer-ys (->> (get board 0)
                               vals
                               (mapcat keys))
            min-base-layer-y (apply min base-layer-ys)
            max-base-layer-y (apply max base-layer-ys)
            board-height  (inc (- max-base-layer-y min-base-layer-y))
            view-layer @(subscribe [::view-layer])
            view-width @(subscribe [::view-width])
            view-height @(subscribe [::view-height])
            scale (min (/ view-height board-height TILE_HEIGHT)
                       (/ view-width board-width TILE_WIDTH))
            tile-width (* TILE_WIDTH scale)
            tile-height (* TILE_HEIGHT scale)
            tile-thickness (* TILE_THICKNESS scale)] 
        (letfn [;; (0,0) should be at width/2-sz/2,height/2-sz/2
                (calc-left [x]
                  (+ (/ view-width 2) (* x tile-width) (/ tile-width 2)))
                (calc-top [y layer-idx]
                  (+ (- (+ (/ view-height 2) (* y -1 tile-height))
                        tile-height
                        (/ tile-height 2)
                        (* tile-thickness layer-idx))
                     SETTINGS_PANEL_HEIGHT
                     (/ BOTTOM_PADDING 2)
                     (* SCALE_PADDING 2)))]
          [:<>
           [settings-panel]
           (doall
            (for [x (range (* -1 (/ board-width 2)) (/ board-width 2))]
              (doall
               (for [y (range min-base-layer-y (inc max-base-layer-y))]
                 (let [left (calc-left x)
                       top (+ (calc-top y 0) tile-thickness)]
                   [:div.solitaire-grid {:key (str x y)
                                        :style {:width (str tile-width "px")
                                                :height (str tile-height "px")
                                                :left left
                                                :top top}}])))))
           (doall
            (for [[layer-idx layer] board]
              (doall
               (for [[x ys] layer]
                 (doall
                  (for [[y] ys]
                    (let [left (calc-left x)
                          top (calc-top y layer-idx)
                          face-down? (or (and (get-in layer [(dec x) y]) ;; a tile is face down if there are tiles on both its left & right, or if there's a tile above it
                                              (get-in layer [(inc x) y]))
                                         (get-in board [(inc layer-idx) x y]))]
                      [:div {:key (str x y)
                             :style {:position :absolute
                                     :opacity (if (> layer-idx view-layer) 0 1)
                                     :width (str tile-width "px")
                                     :height (str (+ tile-height tile-thickness) "px")
                                     :left left
                                     :top top
                                     :z-index (+ layer-idx 2)
                                     :display :flex
                                     :flex-direction :column}}
                       [:div {:style {:background (get-in COLORS [layer-idx 0])
                                      :border "0.5px solid black"
                                      :box-sizing "border-box"
                                      :width (str tile-width "px")
                                      :height (str tile-height "px")
                                      :padding 2}}
                        (when-not face-down?
                          [:div {:style {:background "rgba(255,255,255,0.5)"
                                         :width "100%"
                                         :height "100%"}}])] 
                       (when-not (get-in layer [x (dec y)])
                         ;; only show side if no tile is below this one
                         [:div {:style {:background (get-in COLORS [layer-idx 1])
                                        :border "0.5px solid black"
                                        :box-sizing "border-box"
                                        :width (str tile-width "px")
                                        :height (str tile-thickness "px")}}])])))))))
           #_(doall ;; abandoned grid x/y labels -- probably won't work in 2-symmetry mode
              (for [x (range (* -1 (/ board-width 2)) (/ board-width 2))]
                (doall
                 (for [y (range (* -1 (/ board-height 2)) (/ board-height 2))]
                   (let [left (calc-left x)
                         top (calc-top y 0)
                         x' (+ x (/ board-width 2))
                         y' (dec (+ (* y -1) (/ board-height 2)))]
                     [:div.solitaire-grid-labels {:key (str x y)
                                                 :style {:width (str tile-width "px")
                                                         :height (str tile-height "px")
                                                         :left left
                                                         :top top}}
                      (when (or (= x' 0)
                                (= x' (dec board-width)))
                        [:div
                         (inc y')])
                      (when (or (= y' 0)
                                (= y' (dec board-height)))
                        [:div {:style {:place-self :end}}
                         (inc x')])])))))
           [:div.solitaire-view-layer-panel
            [:span.material-symbols-outlined "layers"]
            (doall
             (for [idx (keys board)]
               [:button {:key idx
                         :class (when (= idx view-layer) "btn-orange")
                         :on-click #(dispatch [::view-layer idx])}
                (inc idx)]))]
           [:button.btn-orange.solitaire-generate-btn {:on-click #(dispatch [::generate-board])}
            "Generate"]])))}))

;; ====================================
;; Subscriptions
(reg-grab
 ::solitaire
 (fn [db]
   (:solitaire db)))

(reg-grab
 ::symmetry
 :<- [::solitaire]
 (fn [[solitaire]]
   (get solitaire :symmetry 4)))

(reg-grab
 ::max-layers
 :<- [::solitaire]
 (fn [[solitaire]]
   (get solitaire :max-layers 3)))

(reg-grab
 ::num-tiles
 :<- [::solitaire]
 (fn [[solitaire]]
   (get solitaire :num-tiles 152)))

(reg-grab
 ::board
 :<- [::solitaire]
 (fn [[solitaire]]
   (:board solitaire)))

(reg-grab
 ::view-layer
 :<- [::solitaire]
 (fn [[solitaire]]
   (:view-layer solitaire)))

(reg-grab
 ::view-width
 :<- [::solitaire]
 (fn [[solitaire]]
   (:view-width solitaire)))

(reg-grab
 ::view-height
 :<- [::solitaire]
 (fn [[solitaire]]
   (:view-height solitaire)))

;; ====================================
;; Handlers

(reg-event-x
 ::generate-board
 (fn [db]
   (let [board (generate-board {:symmetry (grab db ::symmetry)
                                :max-layers (grab db ::max-layers)
                                :num-tiles (grab db ::num-tiles)
                                :base-layer {0 {0 true}}})]
     (update db :solitaire merge {:board board
                                 :view-layer (-> board keys count dec)} ))))

(reg-event-x
 ::set-view-bounds
 (fn [db width height]
   (update db :solitaire merge {:view-width width
                               :view-height height})))

(reg-event-x
 ::view-layer
 (fn [db layer-idx]
   (assoc-in db [:solitaire :view-layer] layer-idx)))

(reg-event-x
 ::symmetry
 (fn [db val]
   {:db (assoc-in db [:solitaire :symmetry] val)
    :dispatch [::generate-board]}))

(reg-event-x
 ::max-layers
 (fn [db val]
   {:db (assoc-in db [:solitaire :max-layers] val)
    :dispatch [::generate-board]}))

(reg-event-x
 ::num-tiles
 (fn [db val]
   {:db (assoc-in db [:solitaire :num-tiles] val)
    :dispatch [::generate-board]}))