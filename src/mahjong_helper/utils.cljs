(ns mahjong-helper.utils)

(def suitless? #{"Flower" "N" "E" "W" "S" "J"})

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
    (= (:value val) "Dragon")))
(defn joker?
  [val]
  (if (string? val)
    (= val "J")
    (= (:value val) "Joker")))