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
                     ;; suit not picked in context yet — different letters must
                     ;; resolve to different real suits (that's the whole point
                     ;; of using a distinct letter instead of reusing one)
                     (and (nil? (get context pgroup-suit))
                          (some #(and (not= % pgroup-suit) (= (get context %) tile-suit))
                                ["a" "b" "c"]))
                     {:result false}
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
        (keys patterns)))

(defn resolve-group-str
  "Given a resolved DFS context and a pattern group, the concrete 2-char
   tile string it requires (e.g. \"7B\"), or nil if the group's wild
   number or suit letter hasn't been pinned down by this context."
  [context group]
  (let [[_ val suit] group
        val' (if (or (pv-wild-num? val) (pv-wild-num2? val))
               (get context val)
               val)
        suit' (if (= suit ".") "." (get context suit))]
    (when (and val' suit')
      (str val' suit'))))

(defn groups-with-slots
  "Pairs each pattern-group (parens stripped, in order) with the aligned
   slice of `assignment` — a vector parallel to (pattern-slots pattern)."
  [pattern assignment]
  (first
   (reduce (fn [[acc i] group]
             (let [[mul] group
                   mul (int mul)]
               [(conj acc {:group group :tiles (subvec assignment i (+ i mul))})
                (+ i mul)]))
           [[] 0]
           (pattern-groups (string/replace pattern #"[()]" "")))))

(defn- letter-slot-indices
  "Map from each suit letter (\"a\"/\"b\"/\"c\") used in `pattern` to the
   ordered slot indices (into (pattern-slots pattern)) that belong to it."
  [slots]
  (reduce (fn [acc [i group]]
            (let [suit (nth group 2)]
              (if (#{"a" "b" "c"} suit)
                (update acc suit (fnil conj []) i)
                acc)))
          {}
          (map-indexed vector slots)))

(defn- canonicalize-arrangement
  "Collapses arrangements that are pure relabelings of each other — e.g.
   \"a\" bound to Crak and \"b\" to Dot vs. \"a\" to Dot and \"b\" to
   Crak, when the pattern's a-groups and b-groups have the identical
   shape (same mults/values), so it's really the same match. Letters
   whose slot shapes match are grouped, then within each group renamed
   canonically by sorting on which real suit they're bound to (unbound
   sorts last), so every relabeling of the same underlying match reduces
   to one canonical {:context :assignment}."
  [pattern {:keys [context assignment]}]
  (let [slots (pattern-slots pattern)
        by-letter (letter-slot-indices slots)
        profile (fn [letter] (mapv #(subs (slots %) 0 2) (get by-letter letter)))
        classes (->> (keys by-letter)
                    (group-by profile)
                    vals
                    (filter #(> (count %) 1)))]
    (reduce
     (fn [{:keys [context assignment]} class]
       (let [canonical-names (vec (sort class))
             by-suit (sort-by (fn [l] [(if (get context l) 0 1) (get context l)]) class)
             mapping (zipmap by-suit canonical-names)
             ;; strip the whole class first, then add each mapped binding
             ;; back in — folding dissoc+assoc together per-letter would
             ;; let a later letter's dissoc erase an earlier letter's assoc
             ;; whenever the swap round-trips through the same key
             new-context (reduce (fn [ctx old-letter]
                                   (if-let [suit (get context old-letter)]
                                     (assoc ctx (mapping old-letter) suit)
                                     ctx))
                                 (apply dissoc context class)
                                 class)
             updates (mapcat (fn [old-letter]
                               (map vector
                                    (get by-letter (mapping old-letter))
                                    (map assignment (get by-letter old-letter))))
                             class)]
         {:context new-context
          :assignment (reduce (fn [asn [idx v]] (assoc asn idx v)) assignment updates)}))
     {:context context :assignment assignment}
     classes)))

(def ^:private max-arrangements 24)

(defn find-arrangements
  "All maximal ways (tied for `pattern`'s top ranking against `hand`) to
   match its slots to hand tiles. Each result is {:context ctx :assignment
   [tile-str-or-nil ...]}, assignment parallel to (pattern-slots pattern).
   Uses the same search as rank-pattern, just recording every terminal
   state instead of only the best count. Capped at max-arrangements to
   avoid combinatorial blowup on very flexible patterns."
  ([pattern hand]
   (find-arrangements pattern hand max-arrangements))
  ([pattern hand max-arrangements]
   (let [slots (pattern-slots pattern)
         n-slots (count slots)
         best (rank-pattern pattern hand)
         results (atom #{})]
     (letfn [(dfs [slot-i tiles min-i context matched assignment]
               (when (< (count @results) max-arrangements)
                 (if (or (>= slot-i n-slots) (empty? tiles))
                   (when (= matched best)
                     (swap! results conj {:context (dissoc context :pattern)
                                          :assignment assignment}))
                   (when (>= (+ matched (min (- n-slots slot-i) (count tiles))) best)
                     (let [slot (slots slot-i)
                           same-next? (and (< (inc slot-i) n-slots)
                                           (= slot (slots (inc slot-i))))]
                       (doseq [i (range min-i (count tiles))
                               :when (or (= i min-i)
                                         (not= (tiles i) (tiles (dec i))))]
                         (let [{:keys [result context]}
                               (tile-matches-pattern-group context slot (tiles i))]
                           (when result
                             (dfs (inc slot-i)
                                  (vec-remove tiles i)
                                  (if same-next? i 0)
                                  context
                                  (inc matched)
                                  (assoc assignment slot-i (tiles i))))))
                       (let [run-end (loop [j (inc slot-i)]
                                       (if (and (< j n-slots) (= slot (slots j)))
                                         (recur (inc j))
                                         j))]
                         (dfs run-end tiles 0 context matched assignment)))))))]
       (dfs 0 (vec (sort hand)) 0 {:pattern pattern} 0 (vec (repeat n-slots nil)))
       (->> @results
            (sort-by (comp pr-str :assignment))
            (reduce (fn [{:keys [seen out]} arrangement]
                      (let [canonical (canonicalize-arrangement pattern arrangement)]
                        (if (contains? seen canonical)
                          {:seen seen :out out}
                          {:seen (conj seen canonical) :out (conj out arrangement)})))
                    {:seen #{} :out []})
            :out)))))