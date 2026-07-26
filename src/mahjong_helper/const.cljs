(ns mahjong-helper.const
  (:require [re-re-frame.core :refer [dispatch]]))
 
(def suits {"B" {:color "#4cb970"
                 :color-light "rgba(76, 185, 112, 0.5)"
                 :dragon-name "Green"
                 :name "Bamb"
                 :icon "suit-B"}
            "C" {:color "#d9315d"
                 :color-light "rgba(217, 49, 93, 0.5)"
                 :dragon-name "Red"
                 :name "Crak"
                 :icon "suit-C"}
            "D" {:color "#3682c4"
                 :color-light "rgba(54, 130, 196, 0.5)"
                 :dragon-name "Soap"
                 :name "Dot"
                 :icon "suit-D"}})

(def tile-keys ["1"
                "2"
                "3"
                {:key "B" 
                 :suit? true 
                 :icon "Bamb"}
                "4"
                "5"
                "6"
                {:key "C" 
                 :suit? true
                 :icon "Crak"}
                "7"
                "8"
                "9"
                {:key "D" 
                 :suit? true
                 :icon "Dot"}
                {:key "N" :icon "N"}
                {:key "E" :icon "E"}
                {:key "W" :icon "W"}
                {:key "S" :icon "S"}
                {:key "Dragon" :icon "D"}
                {:key "Flower" :icon "F"}
                {:key "J" :icon "J"}
                {:disabled? false
                 :key [:span.material-symbols-outlined {:style {:font-size "1em"}} "backspace"]
                 :on-click #(dispatch [:backspace])
                 :style {:background "black"
                         :color "white"}}])

(def patterns {"32a30.42b46b" {:id 1 :category "2026"}
               "(12a10.12a16a)3Da42b3Db" {:id 2 :category "2026"}
               "(12a10.12a16a)3Da46b3Db" {:id 3 :category "2026"}
               "3F.(12a10.12a16a)32b46c" {:id 4 :category "2026"}
               "22a20.32b36b(1N.1E.1W.1S.)" {:id 5 :category "2026"}
               "2F.(12a10.12a16a)(12b10.12b16b)(12c10.12c16c)" {:id 6 :category "2026" :closed? true}
               "32a34a46a48a" {:id 10 :category "Evens (2468)"}
               "32a34a46b48b" {:id 11 :category "Evens (2468)"}
               "2F.42a24b26b48a" {:id 12 :category "Evens (2468)"}
               "2E.22a34a36a28a2W." {:id 13 :category "Evens (2468)"}
               "42a3Da48b3Db" {:id 14 :category "Evens (2468)"}
               "3F.22a24a36a48a" {:id 15 :category "Evens (2468)"}
               "(12a14a16a18a)42b1Db42c1Dc" {:id 16 :category "Evens (2468)"}
               "(12a14a16a18a)44b1Db44c1Dc" {:id 17 :category "Evens (2468)"}
               "(12a14a16a18a)46b1Db46c1Dc" {:id 18 :category "Evens (2468)"}
               "(12a14a16a18a)48b1Db48c1Dc" {:id 19 :category "Evens (2468)"}
               "3F.(12a14a16a18a)3F.42b" {:id 20 :category "Evens (2468)"}
               "3F.(12a14a16a18a)3F.44b" {:id 21 :category "Evens (2468)"}
               "3F.(12a14a16a18a)3F.46b" {:id 22 :category "Evens (2468)"}
               "3F.(12a14a16a18a)3F.48b" {:id 23 :category "Evens (2468)"}
               "2F.(12a14a16a)38a(12b14b16b)38b" {:id 24 :category "Evens (2468)" :closed? true}
               "4ra6F.4rb" {:id 30 :category "Any Same Numbers"}
               "4ra1Da3rb1Db4rc1Dc" {:id 31 :category "Any Same Numbers"}
               "2F.4ra2rb4rc2Da" {:id 32 :category "Any Same Numbers"}
               "2F.4ra2rb4rc2Db" {:id 33 :category "Any Same Numbers"}
               "5ra4rb5rc" {:id 40 :category "Quints"}
               "2F.5ra2sa5ta" {:id 41 :category "Quints"}
               "5ra5ia4Db" {:id 42 :category "Quints"}
               "21a32a23a34a45a" {:id 50 :category "Runs"}
               "25a36a27a38a49a" {:id 51 :category "Runs"}
               "3F.4ra(1sa1ta1ua)4va" {:id 52 :category "Runs"}
               "3F.4ra(1sb1tb1ub)4va" {:id 53 :category "Runs"}
               "2ra2sa3rb3sb4tc" {:id 54 :category "Runs"}
               "3ra3sa4ta4ua" {:id 55 :category "Runs"}
               "3ra3sa4tb4ub" {:id 56 :category "Runs"}
               "3F.2ra2sa3ta4Da" {:id 57 :category "Runs"}
               "3F.2ra2sb3ta4Db" {:id 58 :category "Runs"}
               "4ra6F.4sa" {:id 59 :category "Runs"}
               "2F.4ra4sa4ta" {:id 60 :category "Runs"}
               "2F.4ra4sb4tc" {:id 61 :category "Runs"}
               "1ra2sa3ta1rb2sb3tb2uc" {:id 62 :category "Runs" :closed? true}
               "21a33a25a37a49a" {:id 70 :category "Odds (13579)"}
               "21a33a25b37b49c" {:id 71 :category "Odds (13579)"}
               "31a33a43b45b" {:id 72 :category "Odds (13579)"}
               "35a37a47b49b" {:id 73 :category "Odds (13579)"}
               "2N.41a23a45a2S." {:id 74 :category "Odds (13579)"}
               "2N.45a27a49a2S." {:id 75 :category "Odds (13579)"}
               "21a(13a15a17a19a)41b41c" {:id 76 :category "Odds (13579)"}
               "23a(13a15a17a19a)43b43c" {:id 77 :category "Odds (13579)"}
               "25a(13a15a17a19a)45b45c" {:id 78 :category "Odds (13579)"}
               "27a(13a15a17a19a)47b47c" {:id 79 :category "Odds (13579)"}
               "29a(13a15a17a19a)49b49c" {:id 80 :category "Odds (13579)"}
               "3F.21a23a35a4Da" {:id 81 :category "Odds (13579)"}
               "3F.25a27a39a4Da" {:id 82 :category "Odds (13579)"}
               "21a23a31b33b45c" {:id 83 :category "Odds (13579)"}
               "25a27a35b37b49c" {:id 84 :category "Odds (13579)"}
               "41a23a25a27a49a" {:id 85 :category "Odds (13579)"}
               "41a23b25b27b49a" {:id 86 :category "Odds (13579)"}
               "2F.21a23a25a31b31c" {:id 87 :category "Odds (13579)" :closed? true}
               "2F.25a27a29a35b35c" {:id 88 :category "Odds (13579)" :closed? true}
               "2F.(11a13a15a)37a39a3Db" {:id 89 :category "Odds (13579)" :closed? true}
               "4N.3E.3W.4S." {:id 90 :category "Winds & Dragons"}
               "3N.4E.4W.3S." {:id 91 :category "Winds & Dragons"}
               "(1ra1sa1ta1ua)3Da3Db4Dc" {:id 92 :category "Winds & Dragons"}
               "(1rc1sc1tc1uc)3Da3Db4Dc" {:id 93 :category "Winds & Dragons"}
               "3N.41a41b3S." {:id 94 :category "Winds & Dragons"}
               "3N.43a43b3S." {:id 95 :category "Winds & Dragons"}
               "3N.45a45b3S." {:id 96 :category "Winds & Dragons"}
               "3N.47a47b3S." {:id 97 :category "Winds & Dragons"}
               "3N.49a49b3S." {:id 98 :category "Winds & Dragons"}
               "3E.42a42b3W." {:id 99 :category "Winds & Dragons"}
               "3E.44a44b3W." {:id 100 :category "Winds & Dragons"}
               "3E.46a46b3W." {:id 101 :category "Winds & Dragons"}
               "3E.48a48b3W." {:id 102 :category "Winds & Dragons"}
               "3F.4N.3F.4Da" {:id 103 :category "Winds & Dragons"}
               "3F.4E.3F.4Da" {:id 104 :category "Winds & Dragons"}
               "3F.4W.3F.4Da" {:id 105 :category "Winds & Dragons"}
               "3F.4S.3F.4Da" {:id 106 :category "Winds & Dragons"}
               "11a1N.12a2E.13a3W.14a4S." {:id 107 :category "Winds & Dragons"}
               "2F.4N.4S.2Da2Db" {:id 108 :category "Winds & Dragons"}
               "2F.4E.4W.2Da2Db" {:id 109 :category "Winds & Dragons"}
               "2N.3E.(12a10.12a16a)3W.2S." {:id 110 :category "369" :closed? true}
               "33a36a46b49b" {:id 120 :category "369"}
               "33a36a46b49c" {:id 121 :category "369"}
               "23a26a33b36b49c" {:id 122 :category "369"}
               "3F.23a36a29a4Da" {:id 123 :category "369"}
               "3F.23a36a29a4Db" {:id 124 :category "369"}
               "23a26a36b39b(1N.1E.1W.1S.)" {:id 125 :category "369"}
               "2F.(23a16a19a)43b43c" {:id 126 :category "369"}
               "2F.(26a16a19a)46b46c" {:id 127 :category "369"}
               "2F.(29a16a19a)49b49c" {:id 128 :category "369"}
               "2F.33a36a39a(13b16b19b)" {:id 129 :category "369" :closed? true}
               "2N.2E.2W.2S.(1ra1Da)(1rb1Db)(1rc1Dc)" {:id 130 :category "Singles & Pairs" :closed? true}
               "12a14a26a28a12b14b26b28b28c" {:id 131 :category "Singles & Pairs" :closed? true}
               "2F.(23a16a19a)(13b26b19b)(13c16c29c)" {:id 132 :category "Singles & Pairs" :closed? true}
               "2ra2sa2ta2ua2va2wa2xa" {:id 133 :category "Singles & Pairs" :closed? true}
               "21a(13a15a17a)29a21b(13b15b17b)29b" {:id 134 :category "Singles & Pairs" :closed? true}})

(def WILDS1 "rstuvwxyz")
(def WILDS2 "ijklmnopq")
(def ALL_WILDS (str WILDS1 WILDS2))

(def all-tiles (into {} (concat
                         ;; 1–9B,1–9C,1–9D x 4 of each
                         (->> suits
                              keys
                              (mapcat (fn [suit]
                                        (->> (range 1 10)
                                             (map #(vector (str % suit) 4))))))
                         ;; Dragons B,C,D x 4 of each
                         (->> suits
                              keys
                              (map #(vector (str "D" %) 4)))
                         ;; N,E,W,S x 4 of each
                         (->> ["N" "E" "W" "S"]
                              (map #(vector (str % ".") 4)))
                         [;; Flower x 8
                          ["F." 8]
                          ;; Joker x 8
                          ["J." 8]])))