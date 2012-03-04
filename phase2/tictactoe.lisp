(defparameter *board-size* 9)
(defparameter *players* '((X . 1) (O . -1)))
(defparameter *starting-board* '(nil nil nil nil nil nil nil nil nil))
(defparameter *game-tree* nil)
(defparameter *current-game-state* nil)
(defparameter *board-rankings* (make-hash-table :test #'equal))
(defparameter *game-states* (make-hash-table :test #'equal))

(defun game-state (board player-who-just-moved)
  (list board
        player-who-just-moved
        (generate-moves board player-who-just-moved)))

(defun generate-moves (board player-who-just-moved)
  "Returns a list of game states, one for each empty board cell"
  ; since 1 and -1 are X and 0, multiple by -1 to alternate between them
  (let ((next-player (* -1 player-who-just-moved)))
    (loop for new-board in (new-boards board next-player)
       collect (or (gethash new-board *game-states*) (setf (gethash new-board *game-states*) (game-state new-board next-player))))))

(defun new-boards (board player-making-move)
  "given a board, generates new boards for every empty cell"
  (labels ((add-board (acc processed remaining)
                      (append acc (list (append processed (list player-making-move) remaining))))
           (process-board-cell (processed current remaining acc)
               (cond ((eq (list-length processed) *board-size*) acc)
                     ((null current) (process-board-cell (append processed (list current)) (car remaining) (cdr remaining) (add-board acc processed remaining)))
                     (t (process-board-cell (append processed (list current)) (car remaining) (cdr remaining) acc)))))
    (process-board-cell '() (car board) (cdr board) '())))

(defun x? (player)
  (= (cdr (assoc 'X *players*)) player))

(defmacro with-gs-vars ((game-state &rest vars) &body body)
  "Example usage: (with-gs-vars (game-state moves vars) (body))"
  (let ((evald-game-state-name (gensym)))
    `(let* ((,evald-game-state-name ,game-state)
            ,@(loop for var in vars collect `(,var (,var ,evald-game-state-name))))
           ,@body)))

(defun moves (game-state)
  (caddr game-state))

(defun board (game-state)
  (car game-state))

(defun player (game-state)
  (cadr game-state))

(defun cell-for-display (cell)
  (cond ((eql cell 1) "X")
        ((eql cell -1) "O")
        (t " ")))

(defun row (board rownum)
  (mapcar #'cell-for-display (subseq board (* 3 (1- rownum)) (* 3 rownum))))

(defun print-row (board rownum)
  (format t "~{ ~a ~^|~}" (row board rownum)))

(defun draw-board (board)
  (loop for i from 1 to 3 do
       (print-row board i)
       (when (not (eql i 3))
         (format t "~%-----------~%")))
  (format t "~%"))

;; *** For rankings
(defun set-rankings (game-state)
  (with-gs-vars (game-state board moves player)
    (let ((board-rank (gethash board *board-rankings*)))
      (cond (board-rank board-rank)
            ((winner board) (set-ranking board (winner board)))
            (moves (let ((rank-fun (if (x? player) #'min #'max))) ; x just moved; o is making move
                     (set-ranking board (apply rank-fun (loop for move in moves collect (set-rankings move))))))))))

(defun set-ranking (board value)
  (setf (gethash board *board-rankings*) value))

(defun winner (board)
  (let ((win-conditions '(
                          ;; horizontal wins
                          (0 1 2)
                          (3 4 5)
                          (6 7 8)
                          
                          ;; vertical wins
                          (0 3 6)
                          (1 4 7)
                          (2 5 8)
                         
                          ;; diagonal wins
                          (0 4 8)
                          (6 4 2))))
    (or (loop for win-condition in win-conditions
           when (win-condition-met win-condition board) return (nth (car win-condition) board))
        (if (board-fullp board) 0))))

(defun board-fullp (board)
  (not (position nil board)))

(defun win-condition-met (win-condition board)
  (let ((first  (nth (car win-condition) board))
        (second (nth (cadr win-condition) board))
        (third  (nth (caddr win-condition) board)))
    (and first second third
         (= first second third))))

(defun board-rank (board)
  (if (null board) ;board should only be move on the first iteration
                   ;in max-move
      -2
      (gethash board *board-rankings*)))

;; *** Game flow
(defun initialize-game ()
  (setf *current-game-state* (setf *game-tree* (game-state *starting-board* -1)))
  (set-rankings *game-tree*))

(defun ai-choose-move (game-state)
  (with-gs-vars (game-state moves)
    (setf *current-game-state* (max-move moves))))

(defun ai-move ()
  (ai-choose-move *current-game-state*)
  (end-turn 'ai))

(defun max-move (moves)
  (let ((max nil))
    (labels ((compare (move)
               (if (> (board-rank (board move)) (board-rank (board max)))
                   (setf max move))))
      (mapc #'compare moves))
    max))

(defun end-turn (turn)
  (with-gs-vars (*current-game-state* board)
    (draw-board board)
    (let ((winner (winner board)))
      (cond ((eql winner -1) (format t "You won!~%") (prompt-restart))
            ((eql winner 1) (format t "You lost!~%") (prompt-restart))
            ((eql winner 0) (format t "It was a draw!~%") (prompt-restart))
            (t (if (eql 'ai turn) (human-move) (ai-move)))))))

(defun prompt-restart ()
  (format t "Play again? y/n: ")
  (let ((response (string-downcase (read-line))))
    (cond ((equal "y" response) (restart-game))
          ((equal "n" response))
          (t (format t "Not a valid response, buddy!~%") (prompt-restart)))))

(defun restart-game ()
  (setf *current-game-state* *game-tree*)
  (ai-move))

(defun human-move ()
  (format t "Enter your move, human: ")
  (with-gs-vars ( *current-game-state* board moves)
    (let ((move-position (1- (parse-integer (read-line)))))
      (when (nth move-position board)
        (format t "That position is already taken, stupid meat machine!~%")
        (human-move))
      (setf *current-game-state* (find -1 moves :key #'(lambda (move) (nth move-position (board move)))))
      (end-turn 'human))))
