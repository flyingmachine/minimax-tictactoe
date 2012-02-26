(defparameter *board-size* 9)
(defparameter *players* '((X . 1) (O . -1)))
(defparameter *starting-board* '(nil nil nil nil nil nil nil nil nil))
(defparameter *board-rankings* (make-hash-table))

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
                     (t (append processed (list current)) (car remaining) (cdr remaining) acc))))
    (f '() (car board) (cdr board) '())))

(defun x? (player)
  (= (cadr (assoc 'X *players*)) player))

(defun o? (player)
  (= (cadr (assoc 'O *players*)) player))

(defmacro with-board (game-state &body body)
  `(let ((board ,(board game-state)))
     ,@body))


(defmacro with-all (game-state &body body)
  (with-board (game-state)
    `(let ((moves (caddr ,game-state))
           (player (cadr ,game-state)))
       ,@body)))

(defmacro with-gs-vars (game-state (&rest vars) &body body)
  `(let (,@(loop for var in vars collect (list (',var ,game-state))))
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
  (if (gethash board *board-rankings*)
      (gethash board *board-rankings*)
      (progn
        (if (winner board)
            (winner board)
            (progn
              (let ((rank-fun (if x? #'max #'min)))
                (setf (gethash board *board-rankings*) (apply rank-fun (loop for move in moves collect (set-rankings move))))))))))

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
    ))

