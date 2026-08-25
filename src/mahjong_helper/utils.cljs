(ns mahjong-helper.utils)

(def suitless? #{"F" "N" "E" "W" "S" "J"})

(defn tile-complete? 
  [{:keys [value suit]}]
  (and value
       (or suit (suitless? value))))

(defn tile-map->str
  [{:keys [value suit]
    :or {suit "."}}] 
  (str (subs (or value "") 0 1) suit))

(defn number?*
  [s]
  (not (js/Number.isNaN (int s))))
(defn dragon?
  [val]
  (if (string? val)
    (= val "D")
    (= (:value val) "D")))
(defn joker?
  [val]
  (if (string? val)
    (= val "J")
    (= (:value val) "J")))

(defn read-storage [& [ignore-last-state?]]
  (let [last-state-time (-> "last-state-time"
                            js/window.localStorage.getItem
                            long
                            js/Date.)
        use-last-state? (and (not ignore-last-state?)
                             (<= (- (js/Date.) last-state-time) (* 15 60 1000)))  ;; last state expires after 15 mins
        last-state (when use-last-state?
                     (-> "last-state"
                         js/window.localStorage.getItem
                         js/JSON.parse
                         (js->clj :keywordize-keys true)))
        use-last-state?' (->> last-state
                              :hand
                              vals
                              (filter seq)
                              seq)
        hand' (->> last-state
                   :hand
                   (map (fn [[key val]]
                          [(-> key name int) val]))
                   (into {}))]
    {:theme (keyword (or (js/window.localStorage.getItem "theme") "jordan"))
     :hand (if (>= (count (keys (:hand last-state))) 13)
             hand'
             (zipmap (range (if (:starting-player? last-state) 14 13))
                     (repeat {})))
     :starting-player? (:starting-player? last-state)
     :editing (or (->> hand' ;; select first non-complete tile
                       (sort-by first)
                       (filter (fn [[_ val]]
                                 (not (tile-complete? val))))
                       first
                       first)
                  0)
     :starting-modal-open? (not use-last-state?')}))