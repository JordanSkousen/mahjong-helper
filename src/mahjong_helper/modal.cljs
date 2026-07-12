(ns mahjong-helper.modal)

(defn Modal
  [{:keys [open? title closable? on-close]} & children]
  (when open?
    [:<>
     [:div.backdrop {:on-click #(when closable?
                                  (on-close))}]
     (into [:div.modal
            [:div.modal-title title]]
           children)]))