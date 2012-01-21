;; board should be an array
(defparameter *board-size* 9)
(defparameter *players* '((1 . X) (-1 . O)))

(defun game-state (board player)
  (list board
        player
        (moves board player)))

(defun moves (board player)
  (let ((next-player (* -1 player)))
    (mapcar (lambda (new-board)
            )
          (new-boards board next-player)))

(defun new-boards (board player-making-move)
  (let ((boards '()))
    (labels ((f (processed current remaining)
                (unless (null current)
                  (if current
                      (append (proccessed (list current) remaining))
                    (append (board (list (append (processed (list player-making-move) remaining)))))))
                (f (append (processed) (list current)) (car remaining) (cdr remaining))
                ))
      (f '() (car board) (cdr board)))))

(defun draw-board (board))
