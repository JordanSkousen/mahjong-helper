(ns mahjong-helper.helper.modals.meld
  (:require ["@dnd-kit/react" :refer [DragDropProvider useDraggable
                                      useDroppable]]
            [clojure.string :as string]
            [mahjong-helper.components.modal :refer [Modal]]
            [mahjong-helper.helper.hand-tile :refer [hand-tile-inner
                                                     hand-tile-style]]
            [re-re-frame.core :refer [dispatch subscribe]]))

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
            :title "Drag and drop tiles you've melded"
            :closable? true
            :on-close #(dispatch [:close-meld-modal])}
     [:> DragDropProvider {:on-drag-end (fn [evt]
                                          (when-not (.-canceled evt)
                                            (let [source-id (.. evt -operation -source -id)]
                                              (if-let [target (.. evt -operation -target)]
                                                (when-not (string? source-id) ;; ensure we're not dragging a group member onto a drop zone
                                                  (let [tile-idx (int source-id)
                                                        [group-id id] (string/split (.-id target) #"/")]
                                                    (dispatch [:meld-add-to-group (uuid group-id) (int id) tile-idx])))
                                                (let [[source-group-id source-idx] (string/split source-id #"/")]
                                                  (dispatch [:meld-remove-from-group (uuid source-group-id) (int source-idx)]))))))}
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
              (-> (if tile-idx
                    [:f> draggable-hand-tile {:idx tile-idx
                                              :tile (get hand tile-idx)
                                              :group-id group-id
                                              :id id}]
                    [:f> meld-drop-zone group-id id])
                  (with-meta {:key id}))))]))]
      (when show-invalid?
        [:div {:style {:margin "10px 0"
                       :color :red}}
         "The tiles you've melded are not valid according to official NMJL rules! (Press 'Save' again to ignore.)"])
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