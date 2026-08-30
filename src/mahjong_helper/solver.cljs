(ns mahjong-helper.solver
  (:require [clojure.string :as string]
            [mahjong-helper.const :refer [suits patterns WILDS1 WILDS2 ALL_WILDS]]
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

(defn groups-with-ranges
  "[{:group group-text :start i :end j}] for each literal 3-char group in
   `pattern` (parens stripped), in pattern order, giving the slot index
   range (into (pattern-slots pattern)) it occupies."
  [pattern]
  (first
   (reduce (fn [[acc i] group]
             (let [[mul] group mul (int mul)]
               [(conj acc {:group group :start i :end (+ i mul)}) (+ i mul)]))
           [[] 0]
           (pattern-groups (string/replace pattern #"[()]" "")))))

(defn- try-match-meld
  "Threads a meld's tiles through tile-matches-pattern-group against
   `group`, in order, accumulating context. Returns the final context if
   every tile matches, else nil."
  [context group meld]
  (reduce (fn [ctx tile]
            (let [{:keys [result context]} (tile-matches-pattern-group ctx group tile)]
              (if result context (reduced nil))))
          context
          meld))

(defn- meld-assignments
  "All valid ways to assign every meld in `melds` (each a vector of tile
   strings, e.g. [\"2B\" \"2B\" \"2B\"]) to a distinct pattern-group
   occurrence with matching mult, under one mutually consistent context.
   A meld is a fixed, already-exposed set of tiles: it can only satisfy
   a group requiring *exactly* that many tiles, and once assigned, no
   other meld or hand tile can touch those slots — this is what makes a
   pattern permanently impossible when a meld doesn't fit it anywhere,
   regardless of what's still in hand.

   Returns a seq of {:context ctx :assignment {slot-idx tile-str, ...}}.
   With no melds, this is just [{:context {:pattern pattern} :assignment
   {}}] — the trivial no-op start every hand-only match already used.
   Empty altogether (not even the trivial case) if the melds can't all
   simultaneously fit."
  [pattern melds]
  (let [ranges (groups-with-ranges pattern)]
    (letfn [(search [remaining ranges context assignment]
              (if (empty? remaining)
                [{:context context :assignment assignment}]
                (let [meld (sort (first remaining))
                      mult (count meld)]
                  (mapcat (fn [{:keys [group start end] :as candidate}]
                            (when (= (- end start) mult)
                              (when-let [context' (try-match-meld context group meld)]
                                (search (rest remaining)
                                        (remove #{candidate} ranges)
                                        context'
                                        (into assignment (map vector (range start end) meld))))))
                          ranges))))]
      (search (vec melds) ranges {:pattern pattern} {}))))

(defn- fill-remaining-max
  "Backtracking search filling `remaining-idxs` (positions into `slots`)
   using `tiles` (already sorted), starting from context/matched.
   Returns the best total matched count achievable — never lower than
   the starting `matched`, since that's already a valid outcome (fill
   nothing further). Same branch & bound / duplicate-run-skip as before,
   just over an arbitrary subset of slots instead of always 0..n-1, so
   melds can pre-claim some of them before the hand tiles are tried."
  [slots remaining-idxs tiles context matched]
  (let [n (count remaining-idxs)
        best (atom matched)]
    (letfn [(dfs [pos tiles min-i context matched]
              (swap! best max matched)
              (when (and (< pos n)
                         (seq tiles)
                         (> (+ matched (min (- n pos) (count tiles))) @best))
                (let [slot-idx (remaining-idxs pos)
                      slot (slots slot-idx)
                      same-next? (and (< (inc pos) n)
                                      (= slot (slots (remaining-idxs (inc pos)))))]
                  (doseq [i (range min-i (count tiles))
                          ;; only the first of a run of duplicate tiles
                          :when (or (= i min-i)
                                    (not= (tiles i) (tiles (dec i))))]
                    (let [{:keys [result context]}
                          (tile-matches-pattern-group context slot (tiles i))]
                      (when result
                        (dfs (inc pos)
                             (vec-remove tiles i)
                             (if same-next? i 0)
                             context
                             (inc matched)))))
                  ;; leave this run of identical slots unfilled
                  (let [run-end (loop [j (inc pos)]
                                  (if (and (< j n) (= slot (slots (remaining-idxs j))))
                                    (recur (inc j))
                                    j))]
                    (dfs run-end tiles 0 context matched)))))]
      (dfs 0 tiles 0 context matched)
      @best)))

(defn rank-pattern
  "Max number of tiles from hand (plus any melds — fixed, already-exposed
   sets that must each occupy a whole matching-mult pattern group) that
   can fill pattern's slots under one consistent context. 0 if the melds
   can't all fit this pattern at all, regardless of hand: an exposed
   meld can't be taken back, so a pattern it doesn't fit is permanently
   unreachable, not just currently unmatched."
  ([pattern hand] (rank-pattern pattern hand []))
  ([pattern hand melds]
   (let [slots (pattern-slots pattern)
         n-slots (count slots)
         starts (meld-assignments pattern melds)]
     (if (empty? starts)
       0
       (apply max
              (for [{:keys [context assignment]} starts]
                (fill-remaining-max slots
                                    (vec (remove (set (keys assignment)) (range n-slots)))
                                    (vec (sort hand))
                                    context
                                    (count assignment))))))))

(defn rank-patterns
  "hand is a vec of tile strings like [\"5B\" \"DC\" \"N\" \"J\" \"F\"].
   Returns {pattern ranking}; a higher ranking means the hand is closer
   to that mahjong (ranking = tiles already in place)."
  ([hand] (rank-patterns hand []))
  ([hand melds]
   (into {}
         (map (fn [pattern] [pattern (rank-pattern pattern hand melds)]))
         (keys patterns))))

(defn resolve-group-str
  "Given a resolved DFS context and a pattern group, the concrete 2-char
   tile string it requires (e.g. \"7B\"), or nil if the group's wild
   number or suit letter hasn't been pinned down by this context.

   If the group's own suit letter was never bound (no tile ever matched
   it) but the *other* suit letters already claim 2 of the 3 real
   suits, the third is inferred — different letters must resolve to
   different suits, so with only one real suit left unclaimed, it's the
   only value this letter could possibly take."
  [context group]
  (let [[_ val suit] group
        val' (if (or (pv-wild-num? val) (pv-wild-num2? val))
               (get context val)
               val)
        bound-suit (get context suit)
        other-suits (->> ["a" "b" "c"]
                         (remove #{suit})
                         (keep #(get context %))
                         set)
        suit' (cond
                (= suit ".") "."
                bound-suit bound-suit
                (= 2 (count other-suits)) (first (remove other-suits (set (keys suits)))))]
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

(defn- blank-jokers
  "Exactly which flexible slot a joker happens to land in (or which one
   is left empty instead) isn't meaningful to a player — a joker is
   equally free to fill any of the eligible spots, including ones in a
   different group. Replacing joker entries with nil before comparing
   arrangements collapses all such placements down to whichever real
   tiles were actually used, which is the only thing that differs."
  [assignment]
  (mapv #(when-not (= % "J.") %) assignment))

(defn- canonicalize-duplicate-groups
  "The exact same 3-char group (mult+val+suit) can appear more than once
   in a pattern, not necessarily back-to-back (e.g. \"12a\" showing up
   twice with an unrelated group in between). Those slots are just as
   interchangeable as adjacent ones in the same group — which one ends
   up filled when there isn't enough to fill all of them is arbitrary —
   but the DFS's own dedup only looks at immediately-adjacent slots, so
   non-contiguous repeats of a group slip through. Canonicalizes by
   sorting each group's own slot values back into its own slots (filled
   tiles first, then nils), independent of which specific slot the DFS
   happened to fill."
  [pattern assignment]
  (let [slots (pattern-slots pattern)
        by-group (reduce (fn [acc [i group]] (update acc group (fnil conj []) i))
                         {}
                         (map-indexed vector slots))]
    (reduce (fn [asn idxs]
              (if (< (count idxs) 2)
                asn
                (let [sorted-vals (sort-by (fn [v] [(if v 0 1) v]) (map assignment idxs))]
                  (reduce (fn [asn [idx v]] (assoc asn idx v)) asn (map vector idxs sorted-vals)))))
            assignment
            (vals by-group))))

(defn- wild-family-key
  "Buckets a pattern-group by (wild alphabet, mult) if its val is a wild
   number letter (r-z or i-q), else nil. Groups sharing a bucket use the
   same alphabet and need the same count, so a shift within that bucket
   is (potentially) a valid relabeling — see canonicalize-wild-shift."
  [group]
  (let [[mul val] group]
    (cond
      (pv-wild-num? val) [:wilds1 mul]
      (pv-wild-num2? val) [:wilds2 mul]
      :else nil)))

(defn- canonicalize-wild-shift
  "A pattern like \"4ra4sb4tc\" asks for the same count of three
   *consecutive* numbers, one per (independently free) suit. r/s/t don't
   correspond to specific real numbers, so using real numbers 5,6 to
   satisfy roles r,s is the same underlying match as using them for
   roles s,t instead (r just becomes the next unused number below). Only
   valid when every member of the family shares the same mult — shifting
   would otherwise change how many of a number are actually required.

   Left-aligns each such family so its lowest-alphabet member always
   holds the lowest achieved real number: shift by whatever offset moves
   the lowest-alphabet-position member with any filled slots down to the
   family's own lowest alphabet position, remapping both the assignment
   (each member's slots take over the content `delta` positions ahead of
   it) and every letter's context binding (read via the shared global
   alphabet, since pv-wild-map always binds the whole 9-letter run
   together — this is what keeps unrelated, unshifted letters like a
   family's own higher tail consistent whether or not this pass fires)."
  [pattern {:keys [context assignment]}]
  (let [groups (pattern-groups (string/replace pattern #"[()]" ""))
        indexed (first
                 (reduce (fn [[acc i] group]
                           (let [[mul] group mul (int mul)]
                             [(conj acc {:group group :start i :end (+ i mul)}) (+ i mul)]))
                         [[] 0]
                         groups))
        families (->> indexed
                     (filter #(wild-family-key (:group %)))
                     (group-by #(wild-family-key (:group %)))
                     vals
                     (filter #(> (count %) 1)))]
    (reduce
     (fn [{:keys [context assignment]} members]
       (let [wilds (if (= :wilds1 (first (wild-family-key (:group (first members)))))
                     WILDS1 WILDS2)
             pos-of (fn [m] (string/index-of wilds (nth (:group m) 1)))
             filled? (fn [m] (some some? (subvec assignment (:start m) (:end m))))
             min-pos (apply min (map pos-of members))
             filled-positions (->> members (filter filled?) (map pos-of))]
         (if (empty? filled-positions)
           {:context context :assignment assignment}
           (let [delta (- (apply min filled-positions) min-pos)
                 pos->letter (fn [p] (when (< -1 p (count wilds)) (nth wilds p)))
                 by-pos (zipmap (map pos-of members) members)
                 ;; each member's own suit-letter takes over the suit
                 ;; binding of whichever member is `delta` positions
                 ;; ahead — same idea as the number shift below, but for
                 ;; the group's own written suit-letter (a/b/c) rather
                 ;; than its wild-number letter (r/s/t...)
                 suit-letters (map #(nth (:group %) 2) members)
                 new-context (as-> context ctx
                              (reduce (fn [ctx p]
                                        (let [letter (pos->letter p)
                                              src (pos->letter (+ p delta))
                                              v (when src (get context src))]
                                          (if v (assoc ctx letter v) (dissoc ctx letter))))
                                      ctx
                                      (range (count wilds)))
                              (reduce (fn [ctx m]
                                        (let [src (get by-pos (+ (pos-of m) delta))
                                              v (when src (get context (nth (:group src) 2)))]
                                          (if v (assoc ctx (nth (:group m) 2) v) ctx)))
                                      (apply dissoc ctx suit-letters)
                                      members))
                 updates (mapcat (fn [m]
                                   (let [src (get by-pos (+ (pos-of m) delta))
                                         width (- (:end m) (:start m))]
                                     (map vector
                                          (range (:start m) (:end m))
                                          (if src
                                            (subvec assignment (:start src) (:end src))
                                            (repeat width nil)))))
                                 members)]
             {:context new-context
              :assignment (reduce (fn [asn [idx v]] (assoc asn idx v)) assignment updates)}))))
     {:context context :assignment assignment}
     families)))

(defn- trim-irrelevant-context
  "pv-wild-map binds the *whole* 9-letter alphabet as a side effect of
   binding any one wild letter, even ones the pattern never references
   (e.g. binding \"r\" also binds \"z\", unused here, to some arithmetic
   leftover). canonicalize-wild-shift needs to read through those unused
   letters to compute a correct shift, but leaving them in the final
   context makes two otherwise-identical canonical forms compare unequal
   whenever one path shifted (dropping the out-of-range tail) and
   another didn't (keeping whatever pv-wild-map happened to leave
   there). Trimming down to only the letters the pattern actually uses
   makes that irrelevant leftover disappear from the comparison."
  [pattern context]
  (into {} (filter (fn [[k _]] (or (#{"a" "b" "c"} k) (string/includes? pattern k)))) context))

(defn- canonicalize-arrangement
  "Collapses arrangements that are pure relabelings of each other — e.g.
   \"a\" bound to Crak and \"b\" to Dot vs. \"a\" to Dot and \"b\" to
   Crak, when the pattern's a-groups and b-groups have the identical
   shape (same mults/values), so it's really the same match. Letters
   whose slot shapes match are grouped, then within each group renamed
   canonically by sorting on which real suit they're bound to (unbound
   sorts last), so every relabeling of the same underlying match reduces
   to one canonical {:context :assignment}. Joker placement and
   duplicate-group slot choice are likewise normalized away (see
   blank-jokers and canonicalize-duplicate-groups)."
  [pattern {:keys [context assignment]}]
  (let [slots (pattern-slots pattern)
        by-letter (letter-slot-indices slots)
        profile (fn [letter] (mapv #(subs (slots %) 0 2) (get by-letter letter)))
        classes (->> (keys by-letter)
                    (group-by profile)
                    vals
                    (filter #(> (count %) 1)))
        {:keys [context assignment]}
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
         {:context context :assignment (blank-jokers assignment)}
         classes)
        {:keys [context assignment]}
        (canonicalize-wild-shift pattern {:context context :assignment assignment})]
    {:context (trim-irrelevant-context pattern context)
     :assignment (canonicalize-duplicate-groups pattern assignment)}))

(def ^:private max-arrangements 24)

(defn find-arrangements
  "All maximal ways (tied for `pattern`'s top ranking against `hand` plus
   any `melds` — see rank-pattern) to match its slots to hand tiles.
   Each result is {:context ctx :assignment [tile-str-or-nil ...]},
   assignment parallel to (pattern-slots pattern). Uses the same search
   as rank-pattern, just recording every terminal state instead of only
   the best count. Capped at max-arrangements to avoid combinatorial
   blowup on very flexible patterns. Empty if the melds can't all fit
   this pattern at all."
  ([pattern hand]
   (find-arrangements pattern hand [] max-arrangements))
  ([pattern hand melds]
   (find-arrangements pattern hand melds max-arrangements))
  ([pattern hand melds max-arrangements]
   (let [slots (pattern-slots pattern)
         n-slots (count slots)
         hand (vec (sort hand))
         starts (for [{:keys [context assignment]} (meld-assignments pattern melds)
                     :let [claimed (set (keys assignment))]]
                  {:context context
                   :matched (count assignment)
                   :assignment (reduce (fn [v [idx tile]] (assoc v idx tile))
                                       (vec (repeat n-slots nil))
                                       assignment)
                   :remaining-idxs (vec (remove claimed (range n-slots)))})
         best (if (empty? starts)
                0
                (apply max
                       (for [{:keys [context matched remaining-idxs]} starts]
                         (fill-remaining-max slots remaining-idxs hand context matched))))
         results (atom #{})]
     (doseq [{:keys [context matched assignment remaining-idxs]} starts
             :let [n-remaining (count remaining-idxs)]]
       (letfn [(dfs [pos tiles min-i context matched assignment]
                 (when (< (count @results) max-arrangements)
                   (if (or (>= pos n-remaining) (empty? tiles))
                     (when (= matched best)
                       (swap! results conj {:context (dissoc context :pattern)
                                            :assignment assignment}))
                     (when (>= (+ matched (min (- n-remaining pos) (count tiles))) best)
                       (let [slot-idx (remaining-idxs pos)
                             slot (slots slot-idx)
                             same-next? (and (< (inc pos) n-remaining)
                                             (= slot (slots (remaining-idxs (inc pos)))))]
                         (doseq [i (range min-i (count tiles))
                                 :when (or (= i min-i)
                                           (not= (tiles i) (tiles (dec i))))]
                           (let [{:keys [result context]}
                                 (tile-matches-pattern-group context slot (tiles i))]
                             (when result
                               (dfs (inc pos)
                                    (vec-remove tiles i)
                                    (if same-next? i 0)
                                    context
                                    (inc matched)
                                    (assoc assignment slot-idx (tiles i))))))
                         (let [run-end (loop [j (inc pos)]
                                         (if (and (< j n-remaining) (= slot (slots (remaining-idxs j))))
                                           (recur (inc j))
                                           j))]
                           (dfs run-end tiles 0 context matched assignment)))))))]
         (dfs 0 hand 0 context matched assignment)))
     (->> @results
          (sort-by (comp pr-str :assignment))
          (reduce (fn [{:keys [seen out]} arrangement]
                    (let [canonical (canonicalize-arrangement pattern arrangement)]
                      (if (contains? seen canonical)
                        {:seen seen :out out}
                        {:seen (conj seen canonical) :out (conj out arrangement)})))
                  {:seen #{} :out []})
          :out))))