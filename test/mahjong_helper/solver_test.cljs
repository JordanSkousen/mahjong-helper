(ns mahjong-helper.solver-test
  (:require [cljs.test :refer [deftest is testing]]
            [mahjong-helper.solver :as solver]))

(deftest different-letters-require-different-real-suits
  (testing "a distinct pattern letter can never resolve to a suit another letter already claimed"
    (let [hand ["1C" "3C" "5C" "9C" "W." "J." "3D" "5D" "9D" "E." "1D" "1D" "7C" "9D"]
          pattern "21a33a25b37b49c"
          arrangements (solver/find-arrangements pattern hand)]
      (is (seq arrangements))
      (doseq [{:keys [context]} arrangements]
        (let [suits (vals (select-keys context ["a" "b" "c"]))]
          (is (= (count suits) (count (distinct suits)))
              (str "context reused a suit across letters: " context)))))))

(deftest same-letter-still-forces-same-suit
  (testing "reusing the same letter across groups still requires one consistent suit"
    (is (= 5 (solver/rank-pattern "21a33a" ["1B" "1B" "3B" "3B" "3B"])))
    (is (= 3 (solver/rank-pattern "21a33a" ["1B" "1B" "3C" "3C" "3C"])))))

(deftest three-distinct-suits-hand-still-matches-fully
  (testing "a hand that legitimately uses three different suits still scores a full match"
    (is (= 14 (solver/rank-pattern "21a33a25b37b49c"
                                   ["1B" "1B" "3B" "3B" "3B" "5C" "5C" "7C" "7C" "7C"
                                    "9D" "9D" "9D" "9D"])))))

(deftest mirrored-letter-relabelings-are-collapsed
  (testing "when a and b halves have identical shape, swapping which real
            suit plays \"a\" vs \"b\" is the same match and should only
            be shown once"
    (let [hand ["1C" "3C" "5C" "9C" "W." "J." "3D" "5D" "9D" "E." "1D" "1D" "7C" "9D"]
          pattern "21a(13a15a17a)29a21b(13b15b17b)29b"
          arrangements (solver/find-arrangements pattern hand)]
      (is (= 1 (count arrangements))
          (str "expected the Crak/Dot relabeling to collapse to one arrangement, got: "
               arrangements)))))

(deftest distinct-real-arrangements-are-not-collapsed
  (testing "genuinely different ways to fill the same single-letter group
            are not mistaken for a mirrored relabeling — there's only one
            letter here, so no symmetry class even applies"
    ;; suit B and suit C can each independently satisfy the lone "a"
    ;; group; both are real, distinct alternatives.
    (let [hand ["3B" "3B" "3B" "3C" "3C" "3C"]
          pattern "33a"]
      (is (= 2 (count (solver/find-arrangements pattern hand)))))))

(deftest joker-placement-is-collapsed
  (testing "which eligible slot a joker lands in — even across two
            different groups — isn't meaningful and shouldn't fork the
            results; only the real tiles used should matter"
    (let [hand ["3B" "6B" "6B" "6D" "J." "J." "N." "E." "W." "S." "F." "F." "F."]
          pattern "23a26a36b39b(1N.1E.1W.1S.)"
          arrangements (solver/find-arrangements pattern hand)]
      (is (= 1 (count arrangements))
          (str "expected every joker placement to collapse to one arrangement, got: "
               arrangements)))))

(deftest different-real-tiles-are-not-collapsed-by-joker-blanking
  (testing "blanking jokers for comparison shouldn't accidentally blank
            real ties that happen to also be jokers-free alternatives"
    ;; two different real tiles (5B vs 5C) can each independently fill
    ;; the lone group — a genuine difference, not a joker artifact.
    (let [hand ["5B" "5B" "5B" "5C" "5C" "5C"]
          pattern "35a"]
      (is (= 2 (count (solver/find-arrangements pattern hand)))))))

(deftest non-contiguous-duplicate-group-placement-is-collapsed
  (testing "the same literal group (\"12a\") appearing twice with an
            unrelated group in between is still one interchangeable pool
            — which of the two slots the lone available tile fills
            shouldn't fork the results"
    (let [hand ["2B"]
          pattern "12a10.12a"
          arrangements (solver/find-arrangements pattern hand)]
      (is (= 1 (count arrangements))
          (str "expected both slot choices to collapse to one arrangement, got: "
               arrangements)))))

(deftest reported-2b-placement-case-collapses
  (testing "regression: the exact reported hand/pattern combo where the
            lone 2B could fill either of two non-adjacent \"12a\" slots"
    (let [hand ["F." "F." "2B" "2C" "2C" "6D" "6D" "5B" "5B" "5B" "5B" "5C" "5C"]
          pattern "3F.(12a10.12a16a)32b46c"
          arrangements (solver/find-arrangements pattern hand)]
      ;; two genuinely distinct allocations tie for best (which real suit
      ;; plays "a" vs "b"), but each should appear exactly once — not the
      ;; three results a naive search would produce from 2B's two homes.
      (is (= 2 (count arrangements))
          (str "expected the two 2B slot choices under the same suit "
               "assignment to collapse, got: " arrangements)))))

(deftest wild-number-shift-is-collapsed
  (testing "regression: \"4ra4sb4tc\" wants three consecutive numbers,
            one per free suit — using 5,6 to satisfy r,s is the same
            underlying match as using them for s,t (r just shifts up to
            an unused number), and shouldn't fork the results"
    (let [hand ["F." "F." "2B" "2C" "2C" "6D" "6D" "5B" "5B" "5B" "5B" "5C" "5C"]
          pattern "2F.4ra4sb4tc"
          arrangements (solver/find-arrangements pattern hand)]
      (is (= 1 (count arrangements))
          (str "expected the r/s vs s/t shift to collapse to one arrangement, got: "
               arrangements)))))

(deftest distinct-wild-shift-suit-choices-are-not-collapsed
  (testing "shift-canonicalizing shouldn't accidentally merge genuinely
            different suit choices for the same shifted role"
    ;; suit B and suit C can each independently supply the run of fours
    ;; — real, distinct alternatives, not a shift artifact.
    (let [hand ["5B" "5B" "5B" "5B" "5C" "5C" "5C" "5C"]
          pattern "4ra4sb"]
      (is (= 2 (count (solver/find-arrangements pattern hand)))))))

(deftest sole-remaining-suit-is-inferred-for-display
  (testing "regression: when a pattern's other suit letters already claim
            2 of the 3 real suits, an unbound third letter's suit is
            inferrable — different letters must be different suits, so
            with only Dot left unclaimed, that's the only value \"c\"
            could take, even though no actual Dot tile confirmed it"
    (let [hand ["2D" "4D" "DD" "3C" "4C" "5C" "6C" "7C" "5B" "6B" "7B" "N." "S."]
          pattern "1ra2sa3ta1rb2sb3tb2uc"
          {:keys [context]} (first (solver/find-arrangements pattern hand))
          groups (solver/pattern-groups pattern)
          uc-group (first (filter #(= "c" (subs % 2 3)) groups))]
      (is (= "8D" (solver/resolve-group-str context uc-group))
          (str "context: " context ", group: " uc-group)))))

(deftest suit-is-not-inferred-with-two-unclaimed-suits-remaining
  (testing "no inference when only 1 of the 3 real suits is claimed
            elsewhere — the third letter's suit is genuinely still
            ambiguous between the other two"
    ;; val (\"u\") is resolvable on its own, isolating that it's
    ;; specifically the suit inference (not a missing value) that's
    ;; correctly withheld here
    (let [context {"a" "B" "u" "8"}]
      (is (nil? (solver/resolve-group-str context "2uc"))))))

;; "21a32a23a34a45a" -> 11(a) 222(a) 33(a) 444(a) 5555(a): pairs/triples/
;; quads of 1,2,3,4,5, all one suit. Matches the melding feature's own
;; worked example: melding "32a" gives three 2s, melding "45a" gives
;; four 5s.

(deftest melded-groups-count-toward-ranking
  (testing "a melded group is a fixed, already-exposed set — it counts
            fully toward the ranking without needing any of its tiles
            to also be sitting in hand"
    (let [melds [["2B" "2B" "2B"] ["5B" "5B" "5B" "5B"]]
          hand ["1B" "1B" "3B" "3B" "4B" "4B" "4B"]
          pattern "21a32a23a34a45a"]
      (is (= 14 (solver/rank-pattern pattern hand melds))))))

(deftest meld-forces-suit-for-the-rest-of-the-pattern
  (testing "once a meld pins suit \"a\" to Bamb, hand tiles of a
            different suit can no longer fill \"a\"-lettered slots —
            same rule as two ordinary tiles disagreeing on a suit letter"
    (let [melds [["2B" "2B" "2B"]]
          hand ["1C" "1C" "3C" "3C" "4C" "4C" "4C" "5C" "5C" "5C" "5C"]
          pattern "21a32a23a34a45a"]
      ;; only the meld's own 3 tiles count; every Crak tile is rejected
      ;; since "a" is locked to Bamb
      (is (= 3 (solver/rank-pattern pattern hand melds))))))

(deftest meld-that-cannot-fit-makes-pattern-impossible
  (testing "a meld with no matching-mult group anywhere in the pattern
            makes it permanently unreachable — ranking is 0 regardless
            of how well the rest of the hand otherwise fits"
    (let [melds [["N." "N." "N." "N."]] ;; no wind group exists in this pattern
          hand ["1B" "1B" "2B" "2B" "2B" "3B" "3B" "4B" "4B" "4B" "5B" "5B" "5B" "5B"]
          pattern "21a32a23a34a45a"]
      (is (= 0 (solver/rank-pattern pattern hand melds))))))

(deftest meld-mult-must-match-exactly
  (testing "a meld's tile count must exactly match a group's mult — a
            triple can't partially fill a quad or overfill a pair"
    (let [melds [["2B" "2B" "2B"]]
          ;; only a mult-3 group ('32a', three 2s) exists for value 2;
          ;; there's no mult-2 or mult-4 group of 2s to (mis)match
          pattern "21a32a23a34a45a"]
      (is (= 3 (solver/rank-pattern pattern [] melds))))))

(deftest two-melds-can-share-a-suit-letter-consistently
  (testing "two melds assigned to different groups of the same pattern
            still have to agree on the suit letter they share"
    (let [melds [["2B" "2B" "2B"] ["5B" "5B" "5B" "5B"]]
          pattern "21a32a23a34a45a"]
      (is (= 7 (solver/rank-pattern pattern [] melds))))))

(deftest find-arrangements-respects-melds
  (testing "find-arrangements folds melds into every returned
            arrangement's assignment, occupying exactly the group they
            were matched to"
    (let [melds [["2B" "2B" "2B"] ["5B" "5B" "5B" "5B"]]
          hand ["1B" "1B" "3B" "3B" "4B" "4B" "4B"]
          pattern "21a32a23a34a45a"
          arrangements (solver/find-arrangements pattern hand melds)]
      (is (seq arrangements))
      (doseq [{:keys [assignment]} arrangements]
        (is (= 14 (count (remove nil? assignment))))
        (is (every? #(= "2B" %) (subvec assignment 2 5))
            (str "expected the meld's 2B triple at indices 2-4, got: " assignment))
        (is (every? #(= "5B" %) (subvec assignment 10 14))
            (str "expected the meld's 5B quad at indices 10-14, got: " assignment))))))
