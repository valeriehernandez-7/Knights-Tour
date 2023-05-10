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
  Checks if the size of the board is real. It must be an exact and positive integer greater than 3
  due to the fact that the movement of the knight needs at least 3 squares.
  However, a 3X3 board could never meet the condition of going through all the squares on the board.
  @param board-size exact-integer greater than 3
  @return boolean (true: the board size meets the conditions || false: the size doesn't meet the conditions) or raise-argument-error
|#
(define (valid-size? board-size)
  (cond
    ((not (number? board-size)) (raise-argument-error 'valid-size? "exact-integer greater than 3" board-size))
    (else (and (> board-size 3) (exact-integer? board-size)))
  )
)


#|
  Checks if the position is a list of two non-negative integers and if the position 
  exists on the board using the board size
  @param knight-position list with two non-negative integers (zero and positive) as initial position of the knight with the format '(column row)
  @param board-size exact-integer greater than 3
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

(define board-size 4)
(define knight-position '(0 3))
(define sol 
 '(
    (01 12 07 00)
    (06 09 04 13)
    (15 02 11 08)
    (10 05 14 03)
  )
)

(~r 0 #:min-width 2 #:pad-string "0")

(valid-size? board-size)
(valid-position? knight-position board-size)
(size sol)
(generate-board board-size)
(check-position knight-position sol)
(available? knight-position sol)

(solution board-size knight-position)
(all board-size knight-position)
(test board-size sol)
(paint board-size sol)