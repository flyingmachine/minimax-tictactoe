(ns fms.tictactoe)
(declare generate-moves new-boards)

(defn game-state [player-who-just-moved board]
  (list player-who-just-moved
        board
        (generate-moves player-who-just-moved board)))

;; TODO ensure that this returns a vector
(defn generate-moves [player-who-just-moved board]
  (let [next-player (* -1 player-who-just-moved)]
    (map (partial game-state next-player) (new-boards next-player board))))

(defn new-boards [player-making-move board]
  "given a board, generates new boards for every empty cell"
  (letfn [(add-board [acc processed remaining]
            (conj acc (into (conj processed player-making-move) remaining)))
          (process-board-cell [processed current remaining acc]
            (cond (= (count processed) (count board)) acc
                  (nil? current) (process-board-cell (conj processed current) (first remaining) (rest remaining) (add-board acc processed remaining))
                  true (process-board-cell (conj processed current) (first remaining) (rest remaining) acc)))]
    (process-board-cell [] (first board) (rest board) [])))

(defn x? [player]
  (= player 1))

(defn moves [game-state]
  (last  game-state))

(defn board [game-state]
  (second game-state))

(defn player [game-state]
  (first game-state))

(defn cell-for-display [cell]
  (cond (= cell 1) "X"
        (= cell -1) "O"
        true " "))

(defn row [board rownum]
  (reduce (fn [result current] (conj result (cell-for-display current))) [] (subvec board (* 3 (dec rownum)) (* 3 rownum))))

(defn print-row [board rownum]
  (let [r (row board rownum)]
    (format " %s | %s | %s " (r 0) (r 1) (r 2))))