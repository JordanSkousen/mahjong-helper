(ns mahjong-helper.subs
  (:require [mahjong-helper.utils :refer [tile-complete? tile-map->str]]
            [re-re-frame.core :refer [reg-grab grab]])) 

(reg-grab
 :starting-player?
 (fn [db]
   (:starting-player? db)))

(reg-grab
 :hand-size
 (fn [db]
   (if (grab db :starting-player?) 14 13)))

(reg-grab
 :hand
 (fn [db]
   (:hand db)))

(reg-grab
 :hand-as-strs
 (fn [db]
   (->> (grab db :hand)
        vals
        (map tile-map->str))))

(reg-grab
 :num-completed-tiles
 (fn [db]
   (->> (grab db :hand)
        vals
        (filter tile-complete?)
        count)))

(reg-grab
 :hand-complete?
 (fn [db]
   (>= (grab db :num-completed-tiles) (grab db :hand-size))))

(reg-grab
 :editing-idx
 (fn [db]
   (get db :editing 0)))

(reg-grab
 :editing
 (fn [db]
   (get (grab db :hand) (grab db :editing-idx))))

(reg-grab
 :charleston-modal-open?
 (fn [db]
   (get-in db [:charleston :open?])))

(reg-grab
 :charleston-selection
 (fn [db]
   (get-in db [:charleston :selection])))

(reg-grab
 :reset-modal-open?
 (fn [db]
   (:reset-modal-open? db)))