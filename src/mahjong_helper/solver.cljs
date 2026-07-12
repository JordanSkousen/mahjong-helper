(ns mahjong-helper.solver
  (:require [clojure.string :as string]
            [mahjong-helper.const :refer [patterns WILDS1 WILDS2 ALL_WILDS]]
            [mahjong-helper.utils :refer [number?* joker? dragon?]]))

(defn pv-wild-num?
  [pgroup-val]
  (some #{pgroup-val} (seq WILDS1)))
(defn pv-wild-num2?
  [pgroup-val]
  (some #{pgroup-val} (seq WILDS2)))
(defn pv-wild-map
  ([pgroup-val tile-val]
   (pv-wild-map pgroup-val tile-val false))
  ([pgroup-val tile-val _2?]
   (let [arr (if _2? WILDS2 WILDS1)
         p-idx (string/index-of arr pgroup-val)
         tile-val' (int tile-val)
         start (- tile-val' p-idx)]
     (zipmap arr
             (map str (range start (+ start (count arr) 1)))))))
(defn pattern-groups
  [pattern]
  (->> pattern
       (partition 3)
       (map (partial apply str))))

(defn tile-matches-pattern-group
  [context pattern-group tile]
  (let [{:keys [pattern]} context
        [pgroup-mul pgroup-val pgroup-suit] pattern-group
        [tile-val tile-suit] tile
        val-match (cond
                    ;; tile is joker
                    (and (joker? tile-val)
                         (> (int pgroup-mul) 2))
                    {:result true}
                    ;; tile is number, pattern is r-z val, and none picked in context yet
                    (and (number?* tile-val)
                         (pv-wild-num? pgroup-val)
                         (nil? (get context pgroup-val)))
                    {:result true
                     :context (merge context (pv-wild-map pgroup-val tile-val))}
                    ;; tile is number, pattern is r-z val, and matches one picked in context
                    (and (number?* tile-val)
                         (pv-wild-num? pgroup-val)
                         (= (get context pgroup-val) tile-val))
                    {:result true}
                    ;; tile is number, pattern is i-q val, and none picked in context yet
                    (and (number?* tile-val)
                         (pv-wild-num2? pgroup-val)
                         (nil? (get context pgroup-val)))
                    {:result true
                     :context (merge context (pv-wild-map pgroup-val tile-val true))}
                    ;; tile is number, pattern is i-q val, and matches one picked in context
                    (and (number?* tile-val)
                         (pv-wild-num2? pgroup-val)
                         (= (get context pgroup-val) tile-val))
                    {:result true}
                    ;; pattern 0 = white dragon (soap)
                    (and (= pgroup-val "0")
                         (dragon? tile-val)
                         (= tile-suit "D"))
                    {:result true}
                    ;; tile is specific number
                    (= tile-val pgroup-val)
                    {:result true})
        val-match' (update val-match :result (fn [result]
                                               (cond
                                                 ;; generated wild map has negative val, invalid result
                                                 (some #(<= (int %) 0) (vals (select-keys (:context val-match) ALL_WILDS)))
                                                 false
                                                 ;; generated wild map has a val > 9 that will be used in pattern, invalid result
                                                 (some #(and (> (int (get-in val-match [:context %])) 9)
                                                             (string/includes? pattern %)) ALL_WILDS)
                                                 false

                                                 :else
                                                 result)))
        suit-match (cond
                     ;; suit not applicable — no constraint, nothing to bind
                     (= pgroup-suit ".")
                     {:result true}
                     ;; joker takes any suit without binding it
                     (joker? tile-val)
                     {:result true}
                     ;; suit not picked in context yet
                     (nil? (get context pgroup-suit))
                     {:result true
                      :context (assoc context pgroup-suit tile-suit)}
                     ;; suit picked in context matches
                     (= (get context pgroup-suit) tile-suit)
                     {:result true})
        result (boolean (and (:result val-match') (:result suit-match)))]
    {:result result
     :context (cond-> context
                result (merge (:context val-match') (:context suit-match)))}))

(defn pattern-slots
  "Expand a pattern into one entry per tile it requires. Each entry keeps
   the original 3-char group — the multiplier drives the joker rule."
  [pattern]
  (->> (string/replace pattern #"[()]" "")
       pattern-groups
       (mapcat (fn [group]
                 (let [[mul] group]
                   (repeat (int mul) group))))
       vec))

(defn- vec-remove
  [v i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn rank-pattern
  "Max number of tiles from hand that can fill pattern's slots under one
   consistent context (wild numbers + suit letters). Backtracking with
   branch & bound. Within a run of identical slots, tiles are taken in
   sorted order and a skip jumps the whole run, so interchangeable
   assignments aren't re-explored."
  [pattern hand]
  (let [slots (pattern-slots pattern)
        n-slots (count slots)
        best (atom 0)]
    (letfn [(dfs [slot-i tiles min-i context matched]
              (swap! best max matched)
              (when (and (< slot-i n-slots)
                         (seq tiles)
                         (> (+ matched (min (- n-slots slot-i) (count tiles)))
                            @best))
                (let [slot (slots slot-i)
                      same-next? (and (< (inc slot-i) n-slots)
                                      (= slot (slots (inc slot-i))))]
                  (doseq [i (range min-i (count tiles))
                          ;; only the first of a run of duplicate tiles
                          :when (or (= i min-i)
                                    (not= (tiles i) (tiles (dec i))))]
                    (let [{:keys [result context]}
                          (tile-matches-pattern-group context slot (tiles i))]
                      (when result
                        (dfs (inc slot-i)
                             (vec-remove tiles i)
                             (if same-next? i 0)
                             context
                             (inc matched)))))
                  ;; leave this run of identical slots unfilled
                  (let [run-end (loop [j (inc slot-i)]
                                  (if (and (< j n-slots) (= slot (slots j)))
                                    (recur (inc j))
                                    j))]
                    (dfs run-end tiles 0 context matched)))))]
      (dfs 0 (vec (sort hand)) 0 {:pattern pattern} 0)
      @best)))

(defn rank-patterns
  "hand is a vec of tile strings like [\"5B\" \"DC\" \"N\" \"J\" \"F\"].
   Returns {pattern ranking}; a higher ranking means the hand is closer
   to that mahjong (ranking = tiles already in place)."
  [hand]
  (into {}
        (map (fn [pattern] [pattern (rank-pattern pattern hand)]))
        patterns))