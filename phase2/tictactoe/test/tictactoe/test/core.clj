(ns tictactoe.test.core
  (:use [tictactoe.core])
  (:use [clojure.test]))

(deftest test-cell-for-display
  (is (= (cell-for-display 1) " X "))
  (is (= (cell-for-display -1) " O "))
  (is (= (cell-for-display nil) "   ")))

(deftest test-new-boards
  "It should return all possible boards for the player making the move"
  (is (= (new-boards -1 [1 -1 1, -1 1 -1, 1 nil nil])
         [[1 -1 1, -1 1 -1, 1 -1 nil] [1 -1 1, -1 1 -1, 1 nil -1]])))
