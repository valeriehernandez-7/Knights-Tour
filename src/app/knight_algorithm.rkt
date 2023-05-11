#lang racket

#|
  Sebastián M. Chen Cerdas (https://github.com/seballoll)
  Valerie M. Hernández Fernández (https://github.com/valeriehernandez-7)
  Óscar M. Soto Varela (https://github.com/CAMANEM)

  Knight's Tour (Algorithm)
  Extra-class

  Costa Rica Institute of Technology
  Cartago Central Technology Campus
  Computer Engineering Academic Area

  CE 3104 Languages, Compilers and Interpreters
  Eng. Marco Rivera Meneses, MGP
  Class 01

  I Semester
  2023
|#


(displayln "Knight's Tour 🐴")


#|
  Checks if the size of the board is real. It must be an exact and positive integer greater than 4
  due to the fact that the movement of the knight needs at least 3 squares.
  However, a 3X3 or 4X4 board could never meet the condition of going through all the squares on the board.
  @param board-size exact-integer greater than 4
  @return boolean (true: the board size meets the conditions || false: the size doesn't meet the conditions) or raise-argument-error
|#
(define (valid-size? board-size)
  (cond
    ((not (number? board-size)) (raise-argument-error 'valid-size? "exact-integer greater than 4" board-size))
    (else (and (> board-size 4) (exact-integer? board-size)))
  )
)


#|
  Checks if the position is a list of two non-negative integers and if the position 
  exists on the board using the board size
  @param knight-position list with two non-negative integers (zero and positive) as initial position of the knight with the format '(row column)
  @param board-size exact-integer greater than 4
  @return boolean (true: the position meets the conditions || false: the position doesn't meet the conditions) or raise-argument-error
|#
(define (valid-position? knight-position board-size)
  (cond
    ((or (null? knight-position) (not (list? knight-position)))
      (raise-argument-error 'valid-position? "list" knight-position)
    )
    ((not (equal? (length knight-position) 2))
      (raise-argument-error 'valid-position? "two element list '(a b)" knight-position)
    )
    ((not (and (exact-nonnegative-integer? (first knight-position)) (exact-nonnegative-integer? (second knight-position))))
      (raise-argument-error 'valid-position? "two exact-nonnegative-integer list '(0 0)" knight-position)
    )
    (else (and (< (first knight-position) board-size) (< (second knight-position) board-size)))
  )
)


#|
  Checks if the Knight's tour is posible, using the 
  premise "no complete tours exist for square boards of odd dimensions when the tour starts on an odd position (row + column)".
  To understand the premise check out the article at https://link.springer.com/chapter/10.1007/978-981-13-5802-9_16
  @param knight-position list with two non-negative integers (zero and positive) as initial position of the knight with the format '(row column)
  @param board-size exact-integer greater than 4
  @return boolean (true: even board size or even position (row + column) at odd board size || false: odd position (row + column) at odd board size )
|#
(define (tour? knight-position board-size)
  (cond
    ((even? board-size) #t)
    (else (even? (+ (first knight-position) (second knight-position)))
    )
  )
)

(define (size solution)
  (cond
    ((or (null? solution) (not (list? solution))) (raise-argument-error 'size "list" solution))
    (else (length solution))
  )
)

(define (generate-row size (row '()))
  (cond
    ((equal? (length row) size) row)
    (else (generate-row size (generate-row size (append row (list 0)))))
  )
)

(define (generate-board size (board '()))
  (cond
    ((equal? (length board) size) board)
    (else (generate-board size (append board (list (generate-row size)))))
  )
)

(define (print-col row)
  (cond
    ((null? row) row)
    (else 
      (display " ")(display (~r (car row) #:min-width 2 #:pad-string "0"))(display " ")
      (print-col (cdr row))
    )
  )
)

(define (print-row board)
  (cond
    ((null? board) board)
    (else 
      (display "(")
      (print-col (car board))
      (displayln ")")
      (print-row (cdr board))
    )
  )
)

(define (print-board (board '()))
  (cond
    ((null? board) board)
    (else
      (displayln "(")
      (print-row board)
      (displayln ")")
    )
  )
)

(define (check-col position row (col 0))
  (cond
    ((equal? (second position) col) (car row))
    (else (check-col position (cdr row) (+ col 1)))
  )
)

(define (check-row position board (row 0))
  (cond
    ((equal? (first position) row) (check-col position (car board)))
    (else (check-row position (cdr board) (+ row 1)))
  )
)

(define (check-position position board)
  (cond
    ((or (null? position) (null? board)) (error "check-position arguments must be non-null"))
    (else (check-row position board))
  )
)

(define (available? position board)
  (cond
    ((or (null? position) (null? board)) (error "available? arguments must be non-null"))
    ((not (valid-size? (size board))) (raise-argument-error 'available? "board size doesn't meet the requirements" board))
    ((not (valid-position? position (size board))) (raise-argument-error 'available? "position doesn't meet the requirements" position))
    (else (zero? (check-position position board)))
  )
)


#|
  Filters the possible positions list and removes off-board positions.
  An off-board position has a negative row or column.
  @param edges list of all L-pattern positions
  @param clean list without off-board positions
  @return list of possible L-pattern positions without off-board positions
|#
(define (filter-edges edges (clean '()))
  (cond
    ((null? edges) clean)
    (else 
      (cond
        ((or (negative? (first (car edges))) (negative? (second (car edges)))) (filter-edges (cdr edges) clean))
        (else (filter-edges (cdr edges) (append clean (list (car edges)))))
      )
    )
  )
)


#|
  Retrieves all possible positions by applying the knight movement rule (L-pattern) to the received row's position and col's position.
  @param row non-negative integer [0, board-size]
  @param col non-negative integer [0, board-size]
  @return list of all (8) L-pattern positions
|#
(define (generate-edges row col)
  (cond
    ((or (null? row) (null? col)) (error "generate-edges arguments must be non-null"))
    (else 
      (filter-edges
        (list
          (list (- row 2) (- col 1)) ; 2⬆ 1⬅
          (list (- row 2) (+ col 1)) ; 2⬆ 1➡
          (list (- row 1) (- col 2)) ; 1⬆ 2⬅
          (list (- row 1) (+ col 2)) ; 1⬆ 2➡
          (list (+ row 1) (- col 2)) ; 1⬇ 2⬅
          (list (+ row 1) (+ col 2)) ; 1⬇ 2➡
          (list (+ row 2) (- col 1)) ; 2⬇ 1⬅
          (list (+ row 2) (+ col 1)) ; 2⬇ 1➡
        )
      )
    )
  )
)


#|
  Retrieves the edges (possible positions) of the node (current position)
  @param position list with two non-negative integers (zero and positive) as initial position (node) of the knight with the format '(row column)
  @param board-size exact-integer greater than 4
  @return list of knight's possible positions as edges
|#
(define (get-edges position board-size)
  (cond
    ((or (null? position) (null? board-size)) (error "get-edges arguments must be non-null"))
    ((not (valid-size? board-size)) (raise-argument-error 'get-edges "board-size doesn't meet the requirements" board-size))
    ((not (valid-position? position board-size)) (raise-argument-error 'get-edges "position doesn't meet the requirements" position))
    (else (generate-edges (first position) (second position)))
  )
)


; MAIN FUNCTIONS ------------------------------------------------------------------------------------------------------------------

(define (solution board-size knight-position)
  (display "PDC-Solution")(display "\t board-size ")(display board-size)(display "\t knight-position ")(displayln knight-position)
)

(define (all board-size knight-position)
  (display "PDC-All")(display "\t board-size ")(display board-size)(display "\t knight-position ")(displayln knight-position)
)

(define (test board-size solution)
  (display "PDC-Test")(display "\t board-size ")(display board-size)(display "\t solution ")(displayln solution)
)

(define (paint board-size solution)
  (display "PDC-Paint")(display "\t board-size ")(display board-size)(display "\t solution ")(displayln solution)
)


; TEST ----------------------------------------------------------------------------------------------------------------------------

#|
       0  1  2  3  4
  0  (01 06 15 10 21)
  1  (14 09 20 05 16)
  2  (19 02 07 22 11)
  3  (08 13 24 17 04)
  4  (25 18 03 12 23)
|#

(define board-size 4)
(define knight-position '(0 3))

(define board 
 '(
    (01 06 15 10 21)
    (14 09 20 05 16)
    (19 02 07 22 11)
    (08 13 24 17 04)
    (25 18 03 12 23)
  )
)

(define sol
  '(
    (0 0) (2 1) (4 2) (3 4) (1 3) ; 01 - 05
    (0 1) (2 2) (3 0) (1 1) (0 3) ; 06 - 10
    (2 4) (4 3) (3 1) (1 0) (0 2) ; 11 - 15
    ( ) ( ) ( ) ( ) ( ) ; 16 - 20
    ( ) ( ) ( ) ( ) ( ) ; 21 - 25
  )
)

(valid-size? board-size)
(valid-position? knight-position board-size)
(tour? '(0 0) 5)
(tour? '(1 0) 5)
(size board)
(generate-board board-size)
(print-board board)
(check-position knight-position board)
(available? knight-position board)
(get-edges '(2 2) 5)
(get-edges '(0 0) 5)

(solution board-size knight-position)
(all board-size knight-position)
(test board-size board)
(paint board-size board)