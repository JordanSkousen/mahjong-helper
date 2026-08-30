(ns mahjong-helper.subs
  (:require [mahjong-helper.const :refer [all-tiles]]
            [mahjong-helper.utils :refer [tile-complete? tile-map->str]]
            [re-re-frame.core :refer [reg-grab grab]])) 

(reg-grab
 :starting-modal-open?
 (fn [db]
   (:starting-modal-open? db)))

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
   ;; excludes melded tiles — those are fixed, already-exposed sets
   ;; passed to the solver separately (see :meld-groups-as-strs), not
   ;; part of the free pool it searches over
   (let [melded (set (grab db :melded-tile-idxs))]
     (->> (grab db :hand)
          (remove (fn [[idx _]] (contains? melded idx)))
          vals
          (map tile-map->str)))))

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

(reg-grab
 :result-modal-open-pattern
 (fn [db]
   (:result-modal-open-pattern db)))

(reg-grab
 :preview-mode?
 (fn [db]
   (:preview-mode? db)))

(reg-grab
 :theme
 (fn [db]
   (get db :theme :jordan)))

(reg-grab
 :traditional-theme?
 (fn [db]
   (= (grab db :theme) :traditional)))

(reg-grab
 :svg-url
 (fn [db filename]
   (str "/img/" (name (grab db :theme)) "/" filename ".svg")))

(reg-grab
 :settings-modal-open?
 (fn [db]
   (:settings-modal-open? db)))

(reg-grab
 :creating-tile-disallowed?
 (fn [db tile]
   (when (tile-complete? tile)
     (let [tile-str (tile-map->str tile)
           hand-as-strs (->> (dissoc (grab db :hand) (grab db :editing-idx))
                             vals
                             (filter tile-complete?)
                             (map tile-map->str))
           remaining-tile-counts (reduce (fn [all-tiles' tile-str]
                                           (update all-tiles' tile-str dec))
                                         all-tiles
                                         hand-as-strs)]
       (when-not (nil? (get remaining-tile-counts tile-str))
         (<= (get remaining-tile-counts tile-str) 0))))))

(reg-grab
 :meld-modal-open?
 (fn [db]
   (get-in db [:meld-modal :open?])))

(reg-grab
 :meld-modal-groups
 (fn [db]
   (-> db
       (get-in [:meld-modal :groups])
       (update-vals (fn [group]
                      (let [group' (cond-> group
                                     (>= (->> group
                                              vals
                                              (filter some?)
                                              count) 3) ;; group has 3+ tiles filled, add an empty zone at the end
                                     (assoc (count (keys group)) nil))] 
                        (merge {0 nil
                                1 nil
                                2 nil} ;; there must always be at least 3 zones visible
                               group')))))))

(reg-grab
 :meld-modal-show-invalid?
 (fn [db]
   (get-in db [:meld-modal :show-invalid?])))

(reg-grab
 :meld-groups
 (fn [db]
   (:meld-groups db)))

(reg-grab
 :melded-tile-idxs
 (fn [db]
   (apply concat (vals (grab db :meld-groups)))))

(reg-grab
 :meld-groups-as-strs
 (fn [db]
   ;; each meld group as a vec of tile strings, e.g. ["2B" "2B" "2B"] —
   ;; the shape mahjong-helper.solver/rank-pattern & find-arrangements
   ;; expect for their `melds` argument
   (let [hand (grab db :hand)]
     (->> (grab db :meld-groups)
          vals
          (mapv (fn [idxs] (mapv #(tile-map->str (get hand %)) idxs)))))))