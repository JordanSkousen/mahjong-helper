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
