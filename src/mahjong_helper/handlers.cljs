(ns mahjong-helper.handlers
  (:require [re-re-frame.core :refer [reg-event-x reg-fx grab reg-global-interceptor ->interceptor]]
            [mahjong-helper.utils :refer [tile-complete? suitless? joker?]]))

(reg-event-x
 :initialize-db
 (fn [_ db]
   {:db db}))

(reg-event-x
 :close-starting-modal
 (fn [db]
   (assoc db :starting-modal-open? false)))

(reg-event-x
 :starting-player
 (fn [db starting?]
   (cond-> (assoc db :starting-player? starting?)
     starting? (update :hand assoc 13 {})
     (not starting?) (update :hand dissoc 13))))

;; KEY TYPE RULES
;; ================
;; - if tile is "complete", clear it first
;; - if tile has a suit and we're keying a suitless value, clear it
;; - assoc value/suit
;; - if tile is now "complete", advance to next tile (or stop editing if idx = 12/13)

(reg-event-x
 :key-value
 (fn [db value]
   (let [idx (grab db :editing-idx)
         editing (grab db :editing)
         db' (cond-> db
               (tile-complete? editing) (assoc-in [:hand idx] {})
               (and (suitless? value) (:suit editing)) (assoc-in [:hand idx] {})
               :always (assoc-in [:hand idx :value] value))]
     (when-not (grab db :creating-tile-disallowed? (grab db' :editing))
       {:db db'
        :dispatch [:advance-editing-idx-if-current-editing-tile-complete]}))))

(reg-event-x
 :key-suit
 (fn [db suit]
   (let [idx (grab db :editing-idx)
         editing (grab db :editing)
         db' (cond-> db
               (tile-complete? editing) (assoc-in [:hand idx] {})
               :always (assoc-in [:hand idx :suit] suit))]
     (when-not (grab db :creating-tile-disallowed? (grab db' :editing))
       {:db db'
        :dispatch [:advance-editing-idx-if-current-editing-tile-complete]}))))


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
 :reset-game
 (fn [db]
   (-> db
       (dissoc :hand :editing :starting-player? :reset-modal-open?)
       (assoc :starting-modal-open? true))))

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

(reg-global-interceptor 
 (->interceptor
  :id :save-last-state
  :after (fn [{{:keys [db]} :effects :as context}]
           (cond-> context
             db (assoc-in [:effects ::save-last-state] (select-keys db [:hand :starting-player?]))))))

(reg-fx
 ::save-last-state
 (fn [last-state]
   (js/window.localStorage.setItem "last-state-time" (.valueOf (js/Date.)))
   (->> last-state
        clj->js
        js/JSON.stringify
        (js/window.localStorage.setItem "last-state"))))

(reg-event-x
 :open-meld-modal
 (fn [db]
   (-> (assoc db :meld-modal {:open? true
                              ;; TODO
                              :groups {(random-uuid) {0 nil
                                                      1 nil
                                                      2 nil}}}))))

(reg-event-x
 :close-meld-modal
 (fn [db]
   (dissoc db :meld-modal)))

(reg-event-x
 :meld-add-to-group
 (fn [db meld-group-id id tile-idx]
   (let [db' (-> db
                 (assoc-in [:meld-modal :groups meld-group-id id] tile-idx)
                 (update :meld-modal dissoc :show-invalid?))
         filled-tiles-in-group-count (->> (get-in db' [:meld-modal :groups meld-group-id])
                                          (filter #(some? (val %)))
                                          count)]
     (cond-> db'
       (>= filled-tiles-in-group-count 3) ;; group has 3+ tiles filled, add a new zone
       (assoc-in [:meld-modal :groups meld-group-id filled-tiles-in-group-count] nil)
       
       (every? #(some some? (vals %)) (vals (get-in db' [:meld-modal :groups]))) ;; every group has at least 1 tile filled, create a new blank group
       (assoc-in [:meld-modal :groups (random-uuid)] {0 nil
                                                      1 nil
                                                      2 nil})))))

(reg-event-x
 :meld-remove-from-group
 (fn [db meld-group-id id]
   (let [db' (-> db
                 (assoc-in [:meld-modal :groups meld-group-id id] nil)
                 (update :meld-modal dissoc :show-invalid?))
         completely-empty-groups (->> (get-in db' [:meld-modal :groups])
                                      (filter (fn [[_ v]]
                                                (every? #(nil? (val %)) v))))]
     (cond-> (loop [db'' db']
               (if (and (-> (get-in db'' [:meld-modal :groups meld-group-id])
                            last
                            val
                            (= nil))
                        (-> (get-in db'' [:meld-modal :groups meld-group-id])
                            drop-last
                            last
                            val
                            (= nil)))
                 (recur (update-in db'' [:meld-modal :groups meld-group-id] dissoc (-> (get-in db'' [:meld-modal :groups meld-group-id])
                                                                                       last
                                                                                       key)))
                 db''))

       (> (count completely-empty-groups) 1) ;; 2 groups are completely empty, delete the lastmost one
       (update-in [:meld-modal :groups] dissoc (key (last completely-empty-groups)))))))

(comment
  (let [db @re-frame.db/app-db
        meld-group-id (first (keys (get-in db [:meld-modal :groups])))
        id 4
        db' (-> db
                (assoc-in [:meld-modal :groups meld-group-id id] nil)
                (update :meld-modal dissoc :show-invalid?))]
    (-> (loop [db'' db']
          (if (and (-> (get-in db'' [:meld-modal :groups meld-group-id])
                       last
                       val
                       (= nil))
                   (-> (get-in db'' [:meld-modal :groups meld-group-id])
                       drop-last
                       last
                       val
                       (= nil)))
            (recur (update-in db'' [:meld-modal :groups meld-group-id] dissoc (-> (get-in db'' [:meld-modal :groups meld-group-id])
                                                                                  last
                                                                                  key)))
            db''))
        (get-in [:meld-modal :groups meld-group-id]))))

(reg-event-x
 :meld-create-group
 (fn [db]
   (assoc-in db [:meld-modal :groups (random-uuid)] [])))

(reg-event-x
 :save-meld
 (fn [db]
   (let [ignore-invalid? (grab db :meld-modal-show-invalid?)
         db' (reduce (fn [db' idx]
                       (assoc-in db' [:hand idx :melded?] (get-in db [:meld-modal :selection idx])))
                     db
                     (range (grab db :hand-size)))
         invalid? (when-not ignore-invalid?
                    (let [melded-tiles (->> (grab db' :hand)
                                            vals
                                            (filter :melded?))]
                      (->> melded-tiles
                           (filter #(not (joker? %)))
                           (some (fn [melded-tile]
                                   (< (->> melded-tiles
                                           (filter #(or (= % melded-tile)
                                                        (joker? %)))
                                           count) 3))))))] 
     (if invalid?
       (assoc-in db [:meld-modal :show-invalid?] true)
       {:db db'
        :dispatch [:close-meld-modal]}))))
