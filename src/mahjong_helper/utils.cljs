(ns mahjong-helper.utils)

(def suitless? #{"Flower" "N" "E" "W" "S" "J"})

(defn tile-complete? 
  [{:keys [value suit]}]
  (and value
       (or suit (suitless? value))))