(ns mahjong-helper.worker-pool
  (:require [mahjong-helper.const :refer [patterns]]))

(def num-workers 4)

(defn- js-obj->str-map
  "Converts a flat, string-keyed JS object to a CLJS map WITHOUT using
   js->clj. Under :advanced compilation, js->clj's own internal
   object-type detection can collide with an unrelated ClojureScript
   runtime field that Closure happens to rename to the same short
   property name as one of our own keys — a context map that includes
   the wild-number letter \"o\" (mahjong-helper.const/WILDS2) reliably
   triggered \"[object Object] is not ISeqable\" this way, confirmed via
   a minimal {\"o\" \"11\"} repro in worker_pool_test.cljs. Walking
   Object.keys and reading each value with unchecked-get sidesteps
   js->clj's detection entirely, so it can't collide with anything."
  [obj]
  (reduce (fn [m k] (assoc m k (unchecked-get obj k)))
          {}
          (js->clj (js/Object.keys obj))))

(defn- unpack-arrangement
  "arrangement is {context: {...}, assignment: [...]}. context's own keys
   are single letters (\"a\", \"r\", ...) that the solver looks up as
   plain strings — see js-obj->str-map for why it's converted by hand
   instead of via js->clj."
  [js-arrangement]
  (when js-arrangement
    {:context (js-obj->str-map (unchecked-get js-arrangement "context"))
     :assignment (js->clj (unchecked-get js-arrangement "assignment"))}))

(defn- unpack-results
  "results is a JS object keyed by pattern string (arbitrary text like
   \"23a26a36b39b(1N.1E.1W.1S.)\", not a valid identifier), each value
   {ranking: int, arrangement: {...}}. Walked by hand via unchecked-get
   throughout — js->clj's :keywordize-keys would turn these pattern
   strings into keywords (breaking lookups by the original string
   elsewhere) if applied at the top level, but leaving it off would
   leave `:ranking`/`:arrangement` as unrecognized string keys instead
   of the keywords the rest of the app destructures against."
  [js-results]
  (reduce (fn [m pattern-str]
            (let [entry (unchecked-get js-results pattern-str)]
              (assoc m pattern-str
                     {:ranking (unchecked-get entry "ranking")
                      :arrangement (unpack-arrangement (unchecked-get entry "arrangement"))})))
          {}
          (js->clj (js/Object.keys js-results))))

(defonce ^:private workers
  (mapv (fn [_] (js/Worker. "/js/worker.js")) (range num-workers)))

(defonce ^:private request-id (atom 0))

(defn rank-patterns-async
  "Ranks every pattern in mahjong-helper.const/patterns against `hand`,
   splitting the work across num-workers Web Workers so the main thread
   (and the UI) never blocks on it. Calls on-result once with the merged
   {pattern {:ranking int :arrangement {:context .. :assignment ..}}} map
   — the arrangement is the same shape mahjong-helper.solver/find-arrangements
   returns, precomputed here so Preview Mode doesn't have to redo this
   work synchronously for every pattern on the main thread.

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
                ;; `data` crosses the postMessage boundary from the
                ;; separately-compiled :worker build — each build's
                ;; :advanced-mode property renaming is independent, so
                ;; `.-id`/`.-results` dot-access can look for the wrong
                ;; (locally-renamed) property name in release builds.
                ;; unchecked-get reads by literal string key instead.
                (let [data (.-data e)]
                  (when (= (unchecked-get data "id") id)
                    (.removeEventListener worker "message" handler)
                    (swap! received merge (unpack-results (unchecked-get data "results")))
                    (when (and (= (count @received) (count pattern-keys))
                              (= id @request-id))
                      (on-result @received)))))]
        (.addEventListener worker "message" handler)
        (.postMessage worker (clj->js {:id id :hand hand :patterns (vec chunk)}))))))
