(ns fms.tictactoe)
(declare generate-moves-memo new-boards-memo turn get-input ranking-memo)

(defn game-state [player-who-just-moved board]
  (list player-who-just-moved
        board
        (generate-moves-memo player-who-just-moved board)))

(def game-state-memo (memoize game-state))

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
    (when-let [win (first (drop-while (complement (partial win-condition-met? board)) win-conditions))] (board (first win)))))

(defn ranking [game-state]
  (with-gs-vars [game-state moves player board]
    (let [win (winner board)]
      (cond win win
            moves (let [rank-fun (if (x? player) min max)]
                    (apply rank-fun (map ranking-memo moves)))))))

(def ranking-memo (memoize ranking))

;; Game Flow
(defn start []
  (turn "" (game-state-memo -1 (repeat 9 nil))))

(defn play-again-prompt []
  (println "Play again, puny ape?")
  (let [answer (get-input)]
    (if (= answer "y")
      (start))))

(defn handle-win [game-state]
  (with-gs-vars [game-state board]
    (let [w (winner board)]
      (if (x? w)
        (println "You lost, you sad, fleshy creature!")
        (println "You won... this time!"))))
  (play-again-prompt))

(defn handle-draw [game-state]
  (println "It was a draw! How about that.")
  (play-again-prompt))

(defn ai-choose-move [game-state]
  (first (sort-by ranking-memo > (moves game-state))))

(defn parse-int [s]
  (Integer. (re-find #"[0-9]*" s)))

(defn human-choose-move [game-state]
  (println "Enter your move, human:")
  (with-gs-vars [game-state moves]
    (let [selection (dec (parse-int (get-input)))]
      (if ((board game-state) selection)
        (do (println "That position is already taken, stupid meat machine!")
            (human-choose-move game-state))
        (first (filter (fn [move] (= -1 ((board move) selection))) moves))))))

(defn turn [current-player game-state]
  (if (= "q" game-state)
    (println "Goodbye, weakling!")
    (do (with-gs-vars [game-state board]
          (print-board board)
          (cond (winner board) (handle-win game-state)
                (board-full? board) (handle-draw game-state)
                true (if (= "ai" current-player)
                       (turn "human" (human-choose-move game-state))
                       (turn "ai" (ai-choose-move game-state))))))))

(defn get-input []
  (clojure.string/trim (read-line)))

;; TODO how do I make this work in lein repl?
(try (defn get-input []
       (clojure.string/trim (swank.core/with-read-line-support (read-line))))
     (catch ClassNotFoundException e))