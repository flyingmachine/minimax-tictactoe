;; board should be an array
(defparameter *board-size* 9)
(defparameter *players* '((1 . X) (-1 . O)))
(defparameter *starting-board* '(nil nil nil nil nil nil nil nil nil))

(defun game-state (board player)
  (list board
        player
        (moves board player)))

(defun moves (board player)
  (let ((next-player (* -1 player)))
    (mapcar (lambda (new-board)
              (game-state new-board next-player))
          (new-boards board next-player))))

(defun new-boards (board player-making-move)
  (labels ((add-board (acc processed remaining)
                      (append acc (list (append processed (list player-making-move) remaining))))
           (f (processed current remaining acc)
               (cond ((eq (list-length processed) *board-size*) acc)
                     ((null current) (f (append processed (list current)) (car remaining) (cdr remaining) (add-board acc processed remaining)))
                     (t (append processed (list current)) (car remaining) (cdr remaining) acc))))
    (f '() (car board) (cdr board) '())))

(defun draw-board (board))
