(defparameter *board-size* 9)
(defparameter *players* '((X . 1) (O . -1)))
(defparameter *starting-board* '(nil nil nil nil nil nil nil nil nil))
(defparameter *game-tree* nil)
(defparameter *current-game-state* nil)
(defparameter *board-rankings* (make-hash-table :test #'equal))


(defun game-state (board player)
  (list board
        player
        (generate-moves board player)))

(defun generate-moves (board player)
  "Returns a list of game states, one for each empty board cell"
  (let ((next-player (* -1 player)))
    (mapcar (lambda (new-board)
              (game-state new-board next-player))
          (new-boards board next-player))))

(defun new-boards (board player-making-move)
  "given a board, generates new boards for every empty cell"
  (labels ((add-board (acc processed remaining)
                      (append acc (list (append processed (list player-making-move) remaining))))
           (f (processed current remaining acc)
               (cond ((eq (list-length processed) *board-size*) acc)
                     ((null current) (f (append processed (list current)) (car remaining) (cdr remaining) (add-board acc processed remaining)))
                     (t (f (append processed (list current)) (car remaining) (cdr remaining) acc)))))
    (f '() (car board) (cdr board) '())))

(defun x? (player)
  (= (cdr (assoc 'X *players*)) player))

(defun o? (player)
  (= (cdr (assoc 'O *players*)) player))

(defmacro with-gs-vars ((game-state &rest vars) &body body)
  "Example usage: (with-all (game-state moves vars) (body))"
  `(let ,(loop for var in vars collect `(,var (,var ,game-state)))
     ,@body))

(defun moves (game-state)
  (caddr game-state))

(defun board (game-state)
  (car game-state))

(defun player (game-state)
  (cadr game-state))

(defun cell-for-display (cell)
  (if cell cell " "))

(defun row (board rownum)
  (do ((row '())
       (bottom (* 3 (1- rownum)))
       (top (* 3 rownum))
       (count 0 (1+ count))
       (processed-board board (cdr processed-board)))
      ((eql count 9) (nreverse row))
    (if (and (>= count bottom) (< count top))
        (push (cell-for-display (car processed-board)) row))))

(defun print-row (board rownum)
  (format t "~{ ~a ~^|~}" (row board rownum)))

(defun draw-board (board)
  (loop for i from 1 to 3 do
       (print-row board i)
       (when (not (eql i 3))
         (format t "~%-----------~%"))))

;; replace with cond
;; write macro to create board player and moves vars
(defun set-rankings (game-state)
  (with-gs-vars (game-state board moves player)
    (let ((board-rank (gethash board *board-rankings*)))
      (cond (board-rank board-rank)
            ((winner board) (set-ranking board (winner board)))
            (moves (let ((rank-fun (if (x? player) #'max #'min)))
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

(defun initialize-game ()
  (setf *current-game-state* (setf *game-tree* (game-state *starting-board* -1)))
  (set-rankings *game-tree*))

(defun board-rank (board)
  (if (null board)
      -2
      (gethash board *board-rankings*)))

(defun ai-choose-move (game-state)
  (with-gs-vars (game-state moves)
    (setf *current-game-state* (max-move moves))))

(defun ai-move ()
  (ai-choose-move *current-game-state*)
  (draw-board (board *current-game-state*)))

(defun max-move (moves)
  (let ((max nil))
    (labels ((compare (move)
               (if (> (board-rank (board move)) (board-rank (board max)))
                   (setf max move))))
      (mapc #'compare moves))
    max))
