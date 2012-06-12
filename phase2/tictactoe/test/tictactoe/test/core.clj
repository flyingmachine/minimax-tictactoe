(ns tictactoe.test.core
  (:use [tictactoe.core])
  (:use [clojure.test]))

(deftest test-cell-for-display
  (is (= (cell-for-display 1) " X "))
  (is (= (cell-for-display -1) " O "))
  (is (= (cell-for-display nil) "   ")))


;; TODO it's hard to see what's really going on here
(deftest test-new-boards
  "It should return all possible boards for the player making the move"
  (is (= (new-boards -1 [1 -1 1, -1 1 -1, 1 nil nil])
         [[1 -1 1, -1 1 -1, 1 -1 nil] [1 -1 1, -1 1 -1, 1 nil -1]])))

(deftest test-generate-moves-changes-player
  (is (= (:player (first (generate-moves -1 [1 -1 1, -1 1 -1, 1 -1 nil]))) 1)))

