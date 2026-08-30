(ns mahjong-helper.components.menu-btn
  (:require ["@mui/material/Menu" :default Menu]
            ["@mui/material/MenuItem" :default MenuItem]
            [re-re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as r]))

(defn Menu-Btn []
  (let [!anchor (r/atom nil)]
    (fn []
      (let [page (-> @(subscribe [:current-route]) :data :name)]
        [:<>
         [:a.menu-btn {:on-click #(reset! !anchor (.-currentTarget %))}
          [:span.material-symbols-outlined "menu"]]
         [:> Menu {:anchorEl @!anchor
                   :open (boolean @!anchor)
                   :onClose #(reset! !anchor nil)}
          (doall
           (for [{:keys [key label]} [{:key :home
                                       :label "4 Player Helper"}
                                      {:key :solitaire
                                       :label "Solitaire Generator"}]]
             [:> MenuItem {:key key
                           :selected (= page key)
                           :onClick #(do (dispatch [:goto-page key])
                                         (reset! !anchor nil))}
              label]))]]))))