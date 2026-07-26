(ns mahjong-helper.worker
  (:require [mahjong-helper.solver :refer [rank-pattern]]))

(defn- rank-chunk
  [hand chunk-patterns]
  (reduce (fn [m pattern]
            (assoc m pattern (rank-pattern pattern hand)))
          {}
          chunk-patterns))

(defn- on-message
  [e]
  (let [data ^js (.-data e)
        hand (js->clj (.-hand data))
        chunk-patterns (js->clj (.-patterns data))]
    (js/postMessage (clj->js {:id (.-id data)
                              :results (rank-chunk hand chunk-patterns)}))))

(defn init []
  (js/self.addEventListener "message" on-message))
