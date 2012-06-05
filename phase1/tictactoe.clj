(ns fms.tictactoe)
(declare generate-moves-memo new-boards-memo)

(defn game-state [player-who-just-moved board]
  (list player-who-just-moved
        board
        (generate-moves-memo player-who-just-moved board)))

;; TODO ensure that this returns a vector
(defn generate-moves [player-who-just-moved board]
  (let [next-player (* -1 player-who-just-moved)]
    (map (partial game-state next-player) (new-boards-memo next-player board))))

(def generate-moves-memo (memoize generate-moves))

(defn new-boards [player-making-move board]
  "given a board, generates new boards for every empty cell"
  (letfn [(add-board [acc processed remaining]
            (conj acc (into (conj processed player-making-move) remaining)))
          (process-board-cell [processed current remaining acc]
            (cond (= (count processed) (count board)) acc
                  (nil? current) (process-board-cell (conj processed current) (first remaining) (rest remaining) (add-board acc processed remaining))
                  true (process-board-cell (conj processed current) (first remaining) (rest remaining) acc)))]
    (process-board-cell [] (first board) (rest board) [])))

(def new-boards-memo (memoize new-boards))

(defn x? [player]
  (= player 1))

(defmacro with-gs-vars [[game-state & vars] & body]
  "Example usage: (with-gs-vars (game-state moves vars) (body))"
  (let [evald-game-state-name (gensym)]
    `(let [~evald-game-state-name ~game-state
           ~@(reverse (reduce into () (map (fn [var] `(~var (~var ~evald-game-state-name))) vars)))]
       ~@body)))

(defn moves [game-state]
  (last  game-state))

(defn board [game-state]
  (second game-state))

(defn player [game-state]
  (first game-state))

(defn cell-for-display [cell]
  (cond (= cell 1) " X "
        (= cell -1) " O "
        true "   "))

(defn row [board rownum]
  (reduce (fn [result current] (conj result (cell-for-display current))) [] (subvec board (* 3 (dec rownum)) (* 3 rownum))))

(defn rows [board]
  (partition 3 board))

(defn cols [board]
  (vals (group-by #(mod % 3) board)))

(defn row-display [row]
  (str (apply str (interpose \| (map cell-for-display row))) "\n"))

(defn board-display [board]
  (apply str (interpose "-----------\n" (map row-display (rows board)))))

(defn print-board [board]
  (print (board-display board)))

;; Ranking
(defn board-full? [board]
  (empty? (filter nil? board)))

(defn win-condition-met? [board win-condition]
  (let [p1 (board (first  win-condition))
        p2 (board (second win-condition))
        p3 (board (last   win-condition))]
    (and p1 p2 p3 (= p1 p2 p3))))

(defn winner [board]
  (let [win-conditions '(;; horizontal wins
                         (0 1 2)
                         (3 4 5)
                         (6 7 8)
                         
                         ;; vertical wins
                         (0 3 6)
                         (1 4 7)
                         (2 5 8)
                         
                         ;; diagonal wins
                         (0 4 8)
                         (6 4 2))]
    (or (when-let [win (first (drop-while (complement (partial win-condition-met? board)) win-conditions))] (board (first win)))
        (if (board-full? board) 0))))

(defn ranking [game-state]
  (with-gs-vars [game-state moves player]
    (let [win (winner board)]
      (cond win win
            moves (let [rank-fun (if (x? player) min max)]
                    (apply rank-fun (map ranking moves)))))))

