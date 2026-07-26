(ns mahjong-helper.handlers
  (:require [re-re-frame.core :refer [reg-event-x reg-fx grab]]
            [mahjong-helper.utils :refer [tile-complete?]]))

(reg-event-x
 :initialize-db
 (fn [_ db]
   {:db db}))

(reg-event-x
 :starting-player
 (fn [db starting?]
   (-> db
       (assoc :starting-player? starting?)
       (assoc :hand (zipmap (range (if starting? 14 13))
                            (repeat {}))))))

;; KEY TYPE RULES
;; ================
;; - if tile is "complete", clear it first
;; - assoc value/suit
;; - if tile is now "complete", advance to next tile (or stop editing if idx = 12/13)

(reg-event-x
 :key-value
 (fn [db value]
   (let [idx (grab db :editing-idx)
         editing (grab db :editing)]
     {:db (cond-> db
            (tile-complete? editing) (assoc-in [:hand idx] {})
            :always (assoc-in [:hand idx :value] value))
      :dispatch [:advance-editing-idx-if-current-editing-tile-complete]})))

(reg-event-x
 :key-suit
 (fn [db suit]
   (let [idx (grab db :editing-idx)
         editing (grab db :editing)]
     {:db (cond-> db
            (tile-complete? editing) (assoc-in [:hand idx] {})
            :always (assoc-in [:hand idx :suit] suit))
      :dispatch [:advance-editing-idx-if-current-editing-tile-complete]})))


(reg-event-x
 :advance-editing-idx-if-current-editing-tile-complete
 (fn [db]
   (when (tile-complete? (grab db :editing))
     (let [idx (grab db :editing-idx)
           needed-idxs (->> (grab db :hand)
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
 :backspace
 (fn [db]
   (let [hand-size (grab db :hand-size)
         idx (grab db :editing-idx)
         {:keys [value suit]} (grab db :editing)]
     (cond
       suit (update-in db [:hand idx] dissoc :suit)
       value (update-in db [:hand idx] dissoc :value)
       (> idx 0) (update db :editing dec)
       (= idx 0) (assoc db :editing (dec hand-size))))))

(reg-event-x
 :edit-tile
 (fn [db idx]
   (if (= (grab db :editing-idx) idx)
     (assoc db :editing -1)
     (assoc db :editing idx))))

(reg-event-x
 :edit-prev-tile
 (fn [db]
   (let [editing-idx (grab db :editing-idx)]
     {:dispatch [:edit-tile (if (= editing-idx 0)
                              (dec (grab db :hand-size))
                              (dec editing-idx))]})))

(reg-event-x
 :edit-next-tile
 (fn [db]
   (let [editing-idx (grab db :editing-idx)]
     {:dispatch [:edit-tile (if (= editing-idx (dec (grab db :hand-size)))
                              0
                              (inc editing-idx))]})))

(reg-event-x
 :open-charleston-modal
 (fn [db]
   (-> db
       (assoc :charleston {:open? true})
       (assoc :editing -1))))

(reg-event-x
 :close-charleston-modal
 (fn [db]
   (dissoc db :charleston)))

(reg-event-x
 :toggle-charleston-select
 (fn [db id]
   (let [db' (-> db
                 (update-in [:charleston :selection id] not)
                 (assoc-in [:charleston :selection-time id] (.valueOf (js/Date.))))
         oldest-selected-id (->> (get-in db' [:charleston :selection-time])
                                 (sort-by val)
                                 first
                                 key)]
     (cond-> db'
       (> (->> (get-in db' [:charleston :selection])
               vals
               (filter true?)
               count) 3)
       (-> (assoc-in [:charleston :selection oldest-selected-id] false)
           (update-in [:charleston :selection-time] dissoc oldest-selected-id))))))

(reg-event-x
 :save-charleston
 (fn [db]
   (let [ids (->> (get-in db [:charleston :selection])
                  (filter #(true? (val %)))
                  (map key))
         hand' (apply dissoc (:hand db) ids)
         hand'-count (count (keys hand'))]
     {:db (assoc db :hand (merge (zipmap (range hand'-count)
                                         (vals hand'))
                                 (zipmap (range hand'-count (grab db :hand-size))
                                         (repeat {}))))
      :dispatch-n [[:edit-tile hand'-count]
                   [:close-charleston-modal]]})))

(reg-event-x
 :open-reset-modal
 (fn [db]
   (assoc db :reset-modal-open? true)))

(reg-event-x
 :close-reset-modal
 (fn [db]
   (assoc db :reset-modal-open? false)))

(reg-event-x
 :open-settings-modal
 (fn [db]
   (assoc db :settings-modal-open? true)))

(reg-event-x
 :close-settings-modal
 (fn [db]
   (assoc db :settings-modal-open? false)))

(reg-event-x
 :open-result-modal
 (fn [db pattern]
   (assoc db :result-modal-open-pattern pattern)))

(reg-event-x
 :toggle-preview-mode
 (fn [db]
   (update db :preview-mode? not)))

(reg-event-x
 :theme
 (fn [db theme]
   {:db (assoc db :theme theme)
    ::save-theme theme}))

(reg-fx
 ::save-theme
 (fn [theme]
   (js/window.localStorage.setItem "theme" (name theme))))