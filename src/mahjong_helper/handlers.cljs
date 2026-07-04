(ns mahjong-helper.handlers
  (:require [mahjong-helper.subs :as subs]
            [re-re-frame.core :refer [reg-event-x grab]]
            [mahjong-helper.utils :refer [suitless?]]))

(defn initial-db []
  {:hand (zipmap (range 13)
                 (repeat {}))})

(reg-event-x
 ::initialize-db
 (fn []
   {:db (initial-db)}))

(reg-event-x
 ::key-value
 (fn [db value]
   (let [idx (grab db ::subs/editing-idx)
         {:keys [suit]} (grab db ::subs/editing)
         complete? (suitless? value)]
     (cond-> (-> db
                 (assoc-in [:hand idx :value] value)
                 (update-in [:hand idx] dissoc :suit))
       ;; flowers and winds have no suit OR suit already chosen - tile is complete, move to next tile
       (and complete? (= idx 12)) (assoc :editing -1)
       (and complete? (not= idx 12)) (update :editing inc)))))

(reg-event-x
 ::key-suit
 (fn [db suit]
   (let [idx (grab db ::subs/editing-idx)
         {:keys [value]} (grab db ::subs/editing)
         complete? (and value (not (suitless? value)))]
     (if complete?
       (cond-> (assoc-in db [:hand idx :suit] suit)
         (= idx 12) (assoc :editing -1)
         (not= idx 12) (update :editing inc))
        db))))

(reg-event-x
 ::backspace
 (fn [db]
   (let [idx (grab db ::subs/editing-idx)
         {:keys [value suit]} (grab db ::subs/editing)]
     (cond
       suit (update-in db [:hand idx] dissoc :suit)
       value (update-in db [:hand idx] dissoc :value)
       (> idx 0) (-> db
                     (update :editing dec)
                     (update-in [:hand (dec idx)] dissoc :suit))
       (= idx 0) (-> db
                     (assoc :editing 12)
                     (update-in [:hand 12] dissoc :suit))))))

(reg-event-x
 ::edit-tile
 (fn [db idx]
   (if (= (grab db ::subs/editing-idx) idx)
     (assoc db :editing -1)
     (assoc db :editing idx))))
