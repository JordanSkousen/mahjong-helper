(ns mahjong-helper.handlers
  (:require [mahjong-helper.subs :as subs]
            [re-re-frame.core :refer [reg-event-x grab]]
            [mahjong-helper.utils :refer [tile-complete?]]))

(defn initial-db []
  {:hand (zipmap (range 13)
                 (repeat {}))})

(reg-event-x
 ::initialize-db
 (fn []
   {:db (initial-db)}))

;; KEY TYPE RULES
;; ================
;; - if tile is "complete", clear it first
;; - assoc value/suit
;; - if tile is now "complete", advance to next tile (or stop editing if idx = 12)

(reg-event-x
 ::key-value
 (fn [db value]
   (let [idx (grab db ::subs/editing-idx)
         editing (grab db ::subs/editing)]
     {:db (cond-> db
            (tile-complete? editing) (assoc-in [:hand idx] {})
            :always (assoc-in [:hand idx :value] value))
      :dispatch [::advance-editing-idx-if-current-editing-tile-complete]})))

(reg-event-x
 ::key-suit
 (fn [db suit]
   (let [idx (grab db ::subs/editing-idx)
         editing (grab db ::subs/editing)]
     {:db (cond-> db
            (tile-complete? editing) (assoc-in [:hand idx] {})
            :always (assoc-in [:hand idx :suit] suit))
      :dispatch [::advance-editing-idx-if-current-editing-tile-complete]})))


(reg-event-x
 ::advance-editing-idx-if-current-editing-tile-complete
 (fn [db] 
   (when (tile-complete? (grab db ::subs/editing))
     (let [idx (grab db ::subs/editing-idx)
           needed-idxs (->> (grab db ::subs/hand)
                            (filter (fn [[key tile]]
                                      (not (tile-complete? tile))))
                            (map first)
                            sort)]
       (cond
         ;; all done
         (empty? needed-idxs)
         (assoc db :editing -1)
         ;; next needed is after this one
         (some #(> % idx) needed-idxs)
         (assoc db :editing (->> needed-idxs
                                 (filter #(> % idx))
                                 first))
         ;; next needed is before this one
         :else
         (assoc db :editing (first needed-idxs)))))))

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
