(ns mahjong-helper.worker-pool-test
  (:require [cljs.test :refer [deftest is testing]]
            [mahjong-helper.solver :as solver]
            [mahjong-helper.const :refer [patterns]]))

;; Mirrors mahjong-helper.worker/rank-chunk and mahjong-helper.worker-pool's
;; response unpacking, minus the actual postMessage — js/Worker doesn't
;; exist under node-test, but the exact clj->js / js->clj round trip those
;; two (separately-compiled) builds rely on can still be exercised here.
;; Crucially, `npx shadow-cljs compile test` only ever runs in :dev mode —
;; this file is only useful run via `npx shadow-cljs release test`, which
;; actually applies :advanced optimization the way the real app does.

(defn- rank-one
  [hand pattern]
  (let [arrangement (first (solver/find-arrangements pattern hand 1))
        ranking (count (remove nil? (:assignment arrangement)))]
    {:ranking ranking :arrangement arrangement}))

(defn- js-obj->str-map
  "See mahjong-helper.worker-pool/js-obj->str-map: under :advanced
   compilation, js->clj's own object-type detection can collide with an
   unrelated ClojureScript runtime field Closure renames to the same
   short property name as one of ours (concretely: a context map
   containing wild-letter key \"o\" reliably threw \"[object Object] is
   not ISeqable\"). Walking Object.keys + unchecked-get avoids it."
  [obj]
  (reduce (fn [m k] (assoc m k (unchecked-get obj k)))
          {}
          (js->clj (js/Object.keys obj))))

(defn- unpack-arrangement
  [js-arrangement]
  (when js-arrangement
    {:context (js-obj->str-map (unchecked-get js-arrangement "context"))
     :assignment (js->clj (unchecked-get js-arrangement "assignment"))}))

(defn- unpack-results
  [js-results]
  (reduce (fn [m pattern-str]
            (let [entry (unchecked-get js-results pattern-str)]
              (assoc m pattern-str
                     {:ranking (unchecked-get entry "ranking")
                      :arrangement (unpack-arrangement (unchecked-get entry "arrangement"))})))
          {}
          (js->clj (js/Object.keys js-results))))

(deftest round-trip-preserves-structure-for-every-pattern
  (testing "every pattern's rank-chunk result survives a clj->js/unpack-results
            round trip with the same shape and values it started with —
            regression for a hand that produces a mixed-wild-alphabet
            context (pattern \"5ra5ia4Db\" binds both WILDS1 and WILDS2,
            producing a context whose keys include \"o\")"
    (let [hand ["F." "F." "2B" "2C" "2C" "6D" "6D" "5B" "5B" "5B" "5B" "5C" "5C"]
          pattern-keys (vec (keys patterns))
          direct (into {} (map (fn [p] [p (rank-one hand p)])) pattern-keys)
          js-results (clj->js {:id 1 :results direct})
          round-tripped (unpack-results (unchecked-get js-results "results"))]
      (is (= (count pattern-keys) (count round-tripped)))
      (doseq [pattern pattern-keys]
        (is (= (get direct pattern) (get round-tripped pattern))
            (str "mismatch for " pattern))))))

(deftest o-key-context-round-trips
  (testing "regression: a context map containing the wild-letter key \"o\"
            (from mahjong-helper.const/WILDS2) round-trips correctly —
            this exact shape used to throw \"[object Object] is not
            ISeqable\" under :advanced compilation because js->clj's
            internal object-type detection collided with an unrelated
            renamed ClojureScript runtime field also named \"o\""
    (let [context {"a" "B" "o" "11" "z" "10"}]
      (is (= context (js-obj->str-map (clj->js context)))))))
