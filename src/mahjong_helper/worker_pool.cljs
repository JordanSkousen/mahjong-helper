(ns mahjong-helper.worker-pool
  (:require [mahjong-helper.const :refer [patterns]]))

(def num-workers 4)

(defonce ^:private workers
  (mapv (fn [_] (js/Worker. "/js/worker.js")) (range num-workers)))

(defonce ^:private request-id (atom 0))

(defn rank-patterns-async
  "Ranks every pattern in mahjong-helper.const/patterns against `hand`,
   splitting the work across num-workers Web Workers so the main thread
   (and the UI) never blocks on it. Calls on-result once with the merged
   {pattern ranking} map.

   If a newer call comes in before this one's workers all report back,
   its result is dropped instead of clobbering the fresher one."
  [hand on-result]
  (let [id (swap! request-id inc)
        pattern-keys (vec (keys patterns))
        chunk-size (js/Math.ceil (/ (count pattern-keys) num-workers))
        chunks (partition-all chunk-size pattern-keys)
        received (atom {})]
    (doseq [[worker chunk] (map vector workers chunks)]
      (letfn [(handler [e]
                (let [data (.-data e)]
                  (when (= (.-id data) id)
                    (.removeEventListener worker "message" handler)
                    (swap! received merge (js->clj (.-results data)))
                    (when (and (= (count @received) (count pattern-keys))
                              (= id @request-id))
                      (on-result @received)))))]
        (.addEventListener worker "message" handler)
        (.postMessage worker (clj->js {:id id :hand hand :patterns (vec chunk)}))))))
