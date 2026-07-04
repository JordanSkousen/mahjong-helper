(ns mahjong-helper.router
  (:require [re-re-frame.core :refer [reg-fx reg-event-x dispatch]]
            [reitit.frontend :as rf]
            [reitit.frontend.controllers :as rfc]
            [reitit.frontend.easy :as rfe]))

(def PAGES
  [#_{:id :home
      :view home/Home}])

(defn on-navigate [new-match]
  (when new-match
    (dispatch [::navigated new-match])))

(defn init-router! 
  []
  (-> ["/"
       (->> PAGES
            (map (fn [{:keys [id] :as page}]
                   [(name id) (assoc page :name id)]))
            (into [["" (-> PAGES first (assoc :name :0))]]))]
      rf/router
      (rfe/start! on-navigate {:use-fragment false})))

(reg-event-x
 :goto-page
 (fn [db & route]
   {:push-state route}))

(reg-fx
 :push-state
 (fn [route]
   (apply rfe/push-state route)))

(reg-event-x
 ::navigated
 (fn [db new-match]
   (let [history (get-in db [:ui :history] #{})
         old-match (get-in db [:ui :current-route])
         controllers (rfc/apply-controllers (:controllers old-match) new-match)]
     {:db (-> db
              (assoc-in [:ui :current-route] (assoc new-match :controllers controllers))
              (assoc-in [:ui :history] (conj history (-> new-match :data :name))))
      :fx [(when (not= (:path old-match) (:path new-match))
             [::scroll-to-top])]})))

(reg-fx
 ::scroll-to-top
 (fn []
   (.scrollTo js/window #js {:top 0 :left 0 :behavior "instant"})))