(ns mahjong-helper.helper.worker
  (:require [mahjong-helper.helper.solver :refer [find-arrangements]]))

(defn- rank-chunk
  "For each pattern, the best-arrangement is computed once and the
   ranking derived from it (rather than calling rank-pattern separately),
   since find-arrangements already does that work internally. Preview
   Mode's per-pattern tile display comes along for free instead of
   requiring a second, main-thread pass over every pattern. `melds`
   (each a vec of tile strings) are fixed, already-exposed sets folded
   into every arrangement — see mahjong-helper.solver/rank-pattern."
  [hand melds chunk-patterns]
  (reduce (fn [m pattern]
            (let [arrangement (first (find-arrangements pattern hand melds 1))
                  ranking (count (remove nil? (:assignment arrangement)))]
              (assoc m pattern {:ranking ranking :arrangement arrangement})))
          {}
          chunk-patterns))

(defn- on-message
  [e]
  ;; `data` is a plain object built by clj->js on the other side of the
  ;; postMessage boundary — this build and the main :browser build are
  ;; compiled *separately*, each with its own :advanced-mode property
  ;; renaming, so `.-hand`/`.-patterns`/`.-id` dot-access can silently
  ;; look for the wrong (locally-renamed) property name in release
  ;; builds. unchecked-get reads by literal string key, immune to that.
  (let [data ^js (.-data e)
        hand (js->clj (unchecked-get data "hand"))
        melds (js->clj (unchecked-get data "melds"))
        chunk-patterns (js->clj (unchecked-get data "patterns"))]
    (js/postMessage (clj->js {:id (unchecked-get data "id")
                              :results (rank-chunk hand melds chunk-patterns)}))))

(defn init []
  (js/self.addEventListener "message" on-message))
