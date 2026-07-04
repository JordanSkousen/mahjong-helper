(ns mahjong-helper.subs
  (:require [mahjong-helper.utils :refer [suitless? tile-complete?]]
            [re-re-frame.core :refer [reg-grab grab]]))

(reg-grab
 ::hand
 (fn [db]
   (:hand db)))

(reg-grab
 ::num-completed-tiles
 (fn [db]
   (->> (grab db ::hand)
        vals
        (filter tile-complete?)
        count)))

(reg-grab
 ::editing-idx
 (fn [db]
   (get db :editing 0)))

(reg-grab
 ::editing
 (fn [db]
   (get (grab db ::hand) (grab db ::editing-idx))))
