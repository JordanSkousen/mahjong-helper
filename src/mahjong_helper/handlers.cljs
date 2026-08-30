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
;; - if tile is now "complete", advance to next non-melded tile (or stop editing if idx = 12/13)

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
         {:keys [value suit]} (grab db :editing)
         melded-tile-idxs (grab db :melded-tile-idxs)
         non-melded-idxs (remove (set melded-tile-idxs) (set (range hand-size)))
         prev-non-melded-idx (->> non-melded-idxs
                                  (filter #(< % idx))
                                  (apply max))]
     (cond
       suit (update-in db [:hand idx] dissoc :suit)
       value (update-in db [:hand idx] dissoc :value)
       prev-non-melded-idx (assoc db :editing prev-non-melded-idx)
       (not prev-non-melded-idx) (assoc db :editing (apply max non-melded-idxs))))))

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
       (dissoc :editing :meld-groups :starting-player? :reset-modal-open?)
       (assoc :hand (zipmap (range 0 13)
                            (repeat {})))
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
             db (assoc-in [:effects ::save-last-state] (-> db
                                                           (select-keys [:hand :starting-player? :meld-groups])
                                                           (update :meld-groups update-keys str)))))))

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
                              :groups (if-let [meld-groups (grab db :meld-groups)]
                                        (-> (->> meld-groups
                                                 (map (fn [[id group]]
                                                        [id (zipmap (range (count group))
                                                                    group)]))
                                                 (into {}))
                                            (assoc (random-uuid) {})) ;; add an empty group onto the end
                                        {(random-uuid) {}})}))))

(reg-event-x
 :close-meld-modal
 (fn [db]
   (dissoc db :meld-modal)))

(reg-event-x
 :meld-add-to-group
 (fn [db meld-group-id id tile-idx]
   (let [db' (-> db
                 (assoc-in [:meld-modal :groups meld-group-id id] tile-idx)
                 (update :meld-modal dissoc :show-invalid?))]
     (cond-> db'
       (->> (get-in db' [:meld-modal :groups])
            vals
            (every? #(some some? (vals %)))) ;; every group has at least 1 tile filled, create a new blank group
       (assoc-in [:meld-modal :groups (random-uuid)] {})))))

(reg-event-x
 :meld-remove-from-group
 (fn [db meld-group-id id]
   (let [db' (-> db
                 (assoc-in [:meld-modal :groups meld-group-id id] nil)
                 (update :meld-modal dissoc :show-invalid?))
         completely-empty-groups (->> (get-in db' [:meld-modal :groups])
                                      (filter (fn [[_ v]]
                                                (every? #(nil? (val %)) v))))]
     (cond-> db'
       (> (count completely-empty-groups) 1) ;; 2 groups are completely empty, delete the lastmost one
       (update-in [:meld-modal :groups] dissoc (key (last completely-empty-groups)))))))

(reg-event-x
 :save-meld
 (fn [db]
   (let [ignore-invalid? (grab db :meld-modal-show-invalid?)
         hand (grab db :hand)
         groups (->> (grab db :meld-modal-groups)
                      (filter (fn [[_ group]]
                                ;; ignore empty groups
                                (some some? (vals group))))
                      (map (fn [[id group]]
                             ;; map to tiles; ignore empty drop zones
                             [id (->> group
                                      vals
                                      (filter some?))]))
                      (into {}))
         valid? (if ignore-invalid?
                  true
                  (let [groups' (->> groups
                                     vals
                                     (map (fn [group]
                                            (->> group
                                                 (map #(get hand %))))))]
                    (every? (fn [group]
                              (and (>= (count group) 3) ;; meld must be 3 tiles 
                                   (->> group
                                        (filter #(not (joker? %)))
                                        set
                                        count
                                        (= 1)) ;; all tiles in meld must be the same (excluding jokers ofc)
                                   ))
                            groups')))]
     (if-not valid?
       (assoc-in db [:meld-modal :show-invalid?] true)
       {:db (assoc db :meld-groups groups)
        :dispatch [:close-meld-modal]}))))