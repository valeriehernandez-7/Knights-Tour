#lang racket

(require "knight_gui.rkt")

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


(displayln "\nKnight's Tour 🐴\n")


#|
  Checks if the size of the board is real. It must be an exact and positive integer greater than 4
  due to the fact that the movement of the knight needs at least 3 squares.
  However, a 3X3 or 4X4 board could never meet the condition of going through all the squares on the board.
  @param board-size exact-integer greater than 4
  @return boolean (true: the board size meets the conditions || false: the size doesn't meet the conditions) or raise-argument-error
|#
(define (valid-size? board-size)
  (cond
    ((not (number? board-size)) (raise-argument-error 'kt-valid-size? "exact-integer greater than 4" board-size))
    (else (and (> board-size 4) (exact-integer? board-size)))
  )
)


#|
  Checks if the position is a list of two non-negative integers and if the position exists on the board using the board size
  @param knight-position list with two non-negative integers (zero and positive) as initial position of the knight with the format '(row column)
  @param board-size exact-integer greater than 4
  @return boolean (true: the position meets the conditions || false: the position doesn't meet the conditions) or raise-argument-error
|#
(define (valid-position? knight-position board-size)
  (cond
    ((or (null? knight-position) (not (list? knight-position)))
      (raise-argument-error 'kt-valid-position? "list" knight-position)
    )
    ((not (equal? (length knight-position) 2))
      (raise-argument-error 'kt-valid-position? "two element list '(a b)" knight-position)
    )
    ((not (and (exact-nonnegative-integer? (first knight-position)) (exact-nonnegative-integer? (second knight-position))))
      (raise-argument-error 'kt-valid-position? "two exact-nonnegative-integer list '(0 0)" knight-position)
    )
    (else (and (< (first knight-position) board-size) (< (second knight-position) board-size)))
  )
)


#|
  Checks if the solution is a square board complete tour. To be a complete tour the provided solution
  must have (size * size) elements, this means all the board positions are visited.
  @param board-size exact-integer greater than 4
  @param solution pair (row column) list as the solution structure
  @return boolean (true: the solution meets the conditions || false: the solution doesn't meet the conditions) or raise-argument-error
|#
(define (valid-solution? board-size solution)
  (cond
    ((or (null? board-size) (null? solution)) (error "kt-valid-solution? arguments must be non-null"))
    ((not (valid-size? board-size)) (raise-argument-error 'kt-valid-solution? "board-size doesn't meet the requirements" board-size))
    (else (equal? (length solution) (expt board-size 2)))
  )
)


#|
  Checks if the move int is in range of board size.
  @param move positive exact-integer as solution's move (n)
  @param board matrix as the square board
  @return boolean (true: the move meets the conditions || false: the move doesn't meet the conditions)
|#
(define (valid-move? move board)
  (and (> move 0) (<= move (expt (length board) 2)))
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
    (else (even? (+ (first knight-position) (second knight-position))))
  )
)


#|
  Checks if the element is part of the array.
  @param element list element
  @param array list
  @return boolean (true: if the element isn't part of array is available | false: if the element isn't part of array is not available)
|#
(define (available? element array)
  (cond
    ((null? array) #t)
    ((equal? element (car array)) #f)
    (else (available? element (cdr array)))
  )
)


#|
  Creates a list of 0 as new row to create the square board.
  @param size exact-integer greater than 4
  @param row empty list
  @return list as row
|#
(define (create-row size (row '()))
  (cond
    ((equal? (length row) size) row)
    (else (create-row size (create-row size (append row (list 0)))))
  )
)


#|
  Creates a matrix structure as the square board of specific size for the Knight's Tour.
  Uses the create-row as aux to parse the matrix to create the rows and append them.
  @param size exact-integer greater than 4
  @param board empty list
  @return matrix as the Knight's Tour board
|#
(define (create-board size (board '()))
  (cond
    ((equal? (length board) size) board)
    (else (create-board size (append board (list (create-row size)))))
  )
)


#|
  Replaces the row element (0) with the solution's move (n).
  @param move positive exact-integer as solution's move (n)
  @param position list with two non-negative integers (zero and positive) as solution move position with the format '(row column)
  @param row as a list for the movement to be added
  @param col integer as col index
  @param row-updated empty list
  @return list as the board row updated
|#
(define (edit-position move position row (col 0) (row-updated '()))
  (cond
    ((equal? (second position) col) (list (append row-updated (list move) (cdr row))))
    (else (edit-position move position (cdr row) (+ col 1) (append row-updated (list (car row)))))
  )
)


#|
  Uses the solution's move number and updates the row using the given position 
  through aux function edit-position.
  @param move positive exact-integer as solution's move (n)
  @param position list with two non-negative integers (zero and positive) as solution move position with the format '(row column)
  @param board matrix as the square board
  @param row integer as row index
  @param board-updated empty list
  @return matrix as the solution matrix form
|#
(define (assign-move move position board (row 0) (board-updated '()))
  (cond
    ((equal? (first position) row) (append board-updated (edit-position move position (car board)) (cdr board)))
    (else (assign-move move position (cdr board) (+ row 1) (append board-updated (list (car board)))))
  )
)


#|
  Retrieves the Knight´s Tour solution and updates a matrix with the solution's move number.
  Assumes that the solution is presented with the pairs in the order that the Knight visits the position, 
  with the first pair being the initial position and the last pair being the final position.
  @param solution pair (row column) list as the solution structure
  @param board matrix to display the solution matrix form
  @param move positive exact-integer as solution's move (n)
  @return matrix as the solution matrix form
|#
(define (read-solution solution (board '()) (move 1))
  (cond
    ((null? solution) board)
    ((not (valid-move? move board)) (raise-argument-error 'kt-read-solution "move doesn't meet the requirements" move))
    ((not (valid-position? (car solution) (length board))) (raise-argument-error 'kt-read-solution "position doesn't meet the requirements" (car solution)))
    (else (read-solution (cdr solution) (assign-move move (car solution) board) (+ move 1)))
  )
)


#|
  Generates a matrix as the square board of specific 
  size with the given solution only if the requirements are met.
  @param board-size exact-integer greater than 4
  @param solution pair (row column) list as the solution structure
  @return matrix as the solution matrix form
|#
(define (generate-board board-size solution)
  (cond
    ((or (null? board-size) (null? solution)) (error "kt-generate-board arguments must be non-null"))
    ((not (valid-size? board-size)) (raise-argument-error 'kt-generate-board "board-size doesn't meet the requirements" board-size))
    ((not (valid-solution? board-size solution)) (raise-argument-error 'kt-generate-board "solution doesn't meet the requirements" solution))
    (else (read-solution solution (create-board board-size)))
  )
)


#|
  Displays in terminal the row elements with the "##" string format.
  @param digits 
  @param row integer list as board row
  @return the row string format
|#
(define (print-cols digits row)
  (cond
    ((null? row) row)
    (else 
      (display " ")(display (~r (car row) #:min-width digits #:pad-string "0"))(display " ")
      (print-cols digits (cdr row))
    )
  )
)


#|
  Displays in terminal the board rows using string format.
  @param digits 
  @param board integer matrix
  @return matrix as the Knigh's Tour board
|#
(define (print-rows digits board)
  (cond
    ((null? board) board)
    (else 
      (display "  (")
      (print-cols digits (car board))
      (displayln ")")
      (print-rows digits (cdr board))
    )
  )
)


#|
  Displays in terminal the board using print-rows and print-cols as aux functions.
  @param board integer matrix
  @return matrix string format as the Knigh's Tour board
|#
(define (print-board board)
  (cond
    ((null? board) board)
    (else
      (displayln "(")
      (print-rows (+ (exact-floor (log (expt (length board) 2) 10)) 1) board)
      (displayln ")")
    )
  )
)


#|
  Goes through the nodes looking for the chosen node and returns it.
  @param node random integer as the node selected to be the next node
  @param nodes pair list as nodes
  @param node-index integer as node index
  @return pair as the node selected
|#
(define (select-node node nodes (node-index 0))
  (cond
    ((equal? node node-index) (car nodes))
    (else (select-node node (cdr nodes) (+ node-index 1)))
  )
)


#|
  Selects a node-index from the available node list and returns it.
  @param nodes pair list as the current node next nodes
  @return pair as the node selected
|#
(define (random-node nodes)
  (cond
    ((null? nodes) '())
    (else (select-node (random (length nodes)) nodes))
  )
)


#|
  Walk through the graph looking for the node's edges and returns them.
  @param node list with two non-negative integers (zero and positive) as board position with the format '(row column) 
  @param graph as graph matrix form 
  @return pair list as the edges of the node
|#
(define (edges node graph)
  (cond
    ((equal? node (first (car graph))) (second (car graph)))
    (else (edges node (cdr graph)))
  )
)


#|
  Filters the edges returning only the available edges using the available? aux function.
  @param edges pair list as edges
  @param solution pair list as solution
  @param available empty list
  @return list with the available edges
|#
(define (available-edges edges solution (available '()))
  (cond
    ((null? edges) available)
    (else
      (cond
        ((available? (car edges) solution) (available-edges (cdr edges) solution (append available (list (car edges)))))
        (else (available-edges (cdr edges) solution available))
      )
    )
  )
)


#|
  Filters the possible positions list and removes off-board positions.
  An off-board position has either a negative row or column or row or col equal or greater to the board size.
  @param edges list of all L-pattern positions
  @param board-size exact-integer greater than 4
  @param clean list without off-board positions
  @return pair list with possible L-pattern positions without off-board positions
|#
(define (filter-edges edges board-size (clean '()))
  (cond
    ((null? edges) clean)
    (else 
      (cond
        ((or (negative? (first (car edges))) (negative? (second (car edges)))) 
          (filter-edges (cdr edges) board-size clean)
        )
        ((or (>= (first (car edges)) board-size) (>= (second (car edges)) board-size))
          (filter-edges (cdr edges) board-size clean)
        )
        (else (filter-edges (cdr edges) board-size (append clean (list (car edges)))))
      )
    )
  )
)


#|
  Retrieves all possible positions by applying the knight movement rule (L-pattern) to the received row's position and col's position.
  @param row non-negative integer [0, board-size] as row index
  @param col non-negative integer [0, board-size] as col index
  @param board-size exact-integer greater than 4
  @return pair list with all (8) L-pattern positions
|#
(define (generate-edges row col board-size)
  (cond
    ((or (null? row) (null? col)) (error "kt-generate-edges arguments must be non-null"))
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
        board-size
      )
    )
  )
)


#|
  Retrieves the edges (possible positions) of the node (current position).
  @param position list with two non-negative integers (zero and positive) as initial position (node) of the knight with the format '(row column)
  @param board-size exact-integer greater than 4
  @return pair list with the knight's possible positions as edges
|#
(define (get-edges position board-size)
  (cond
    ((or (null? position) (null? board-size)) (error "kt-get-edges arguments must be non-null"))
    ((not (valid-size? board-size)) (raise-argument-error 'kt-get-edges "board-size doesn't meet the requirements" board-size))
    ((not (valid-position? position board-size)) (raise-argument-error 'kt-get-edges "position doesn't meet the requirements" position))
    (else (generate-edges (first position) (second position) board-size))
  )
)


#|
  Creates a square matrix as the graph ((n_1 (e_1 e_n)) (n_n (e_1 e_n))) structure.
  The graph's nodes are all of the square board positions [first-position : (0 0), last-position : (n n)].
  The graph's node edges are all of the L-pattern on-board positions with the node position as the initial position.
  @param board-size exact-integer greater than 4
  @param node position list with two non-negative integers (zero and positive) as first board position with the format '(row column)
  @param graph empty list
  @return matrix as graph matrix form
|#
(define (create-graph board-size (node '(0 0)) (graph '()))
  (cond
    ((= (first node) (second node) (- board-size 1)) 
      (append graph (list (cons node (list (get-edges node board-size)))))
    )
    (else
      (cond
        ((< (second node) (- board-size 1))
          (create-graph
            board-size
            (list (first node) (+ (second node) 1))
            (append graph (list (cons node (list (get-edges node board-size)))))
          )
        )
        (else
          (create-graph
            board-size
            (list (+ (first node) 1) 0)
            (append graph (list (cons node (list (get-edges node board-size)))))
          )
        )
      )
    )
  )
)


#|
  
  @param degrees 
  @param pivot 
  @param less 
  @param equal 
  @param greater 
  @return 
|#
(define (sort-degrees-aux degrees (pivot (second (car degrees))) (less '()) (equal '()) (greater '()))
  (cond 
    ((null? degrees) (append (sort-degrees less) equal (sort-degrees greater)))
    (else
      (cond
        ((> pivot (second (car degrees)))
          (sort-degrees-aux (cdr degrees) pivot (cons (car degrees) less) equal greater)
        )
        ((< pivot (second (car degrees)))
          (sort-degrees-aux (cdr degrees) pivot less equal (cons (car degrees) greater))
        )
        (else (sort-degrees-aux (cdr degrees) pivot less (cons (car degrees) equal) greater))
      )
    )
  )
)


#|
  
  @param degrees 
  @return 
|#
(define (sort-degrees degrees)
  (cond 
    ((null? degrees) '())
    (else (sort-degrees-aux degrees))
  )
)


#|
  
  @param nodes 
  @param graph 
  @param solution 
  @param degrees 
  @return 
|#
(define (nodes-degree nodes graph solution (degrees '()))
  (cond
    ((null? nodes) (sort-degrees degrees))
    (else 
      (nodes-degree
        (cdr nodes)
        graph
        solution
        (cons (append (list (car nodes)) (list (length (available-edges (edges (car nodes) graph) solution)))) degrees)
      )
    )
  )
)


#|
  
  @param degrees 
  @param min-degree 
  @param nodes 
  @return 
|#
(define (nodes-min-degree degrees (min-degree (car degrees)) (nodes '()))
  (cond
    ((null? degrees) nodes)
    ((not (equal? (second min-degree) (second (car degrees)))) nodes)
    (else (nodes-min-degree (cdr degrees) min-degree (cons (first (car degrees)) nodes)))
  )
)


#|
  
  @param nodes 
  @return 
|#
(define (choose-node nodes)
  (cond
    ((> (length nodes) 1) (random-node nodes))
    (else (car nodes))
  )
)


#|
  
  @param node 
  @param available-edges 
  @param graph 
  @param solution 
  @return 
|#
(define (next-node node available-edges graph solution)
  (cond
    ((null? available-edges) node)
    (else (choose-node (nodes-min-degree (nodes-degree available-edges graph solution))))
  )
)


#|
  
  @param board-size 
  @param knight-position 
  @param graph 
  @param solution 
  @return 
|#
(define (create-solution board-size knight-position graph (solution '()))
  (cond
    ((equal? (length solution) (expt board-size 2)) solution)
    (else
      (create-solution 
        board-size 
        (next-node 
          knight-position 
          (available-edges (edges knight-position graph) solution) 
          graph 
          (append solution (list knight-position))
        )
        graph 
        (append solution (list knight-position))
      )
    )
  )
)


#|
  
  @param n 
  @param board-size 
  @param knight-position 
  @param graph 
  @param solutions empty list
  @param solution 
  @return 
|#
(define (create-solutions n board-size knight-position graph (solutions '()) (solution (create-solution board-size knight-position graph)))
  (cond
    ((null? solution) solutions)
    ((equal? (length solutions) n) solutions)
    (else
      (cond
        ((available? solution solutions) (create-solutions n board-size knight-position graph (cons solution solutions)))
        (else (create-solutions n board-size knight-position graph solutions))
      )
    )
  )
)


; MAIN FUNCTIONS ------------------------------------------------------------------------------------------------------------------


#|
  
  @param board-size 
  @param knight-position 
  @return 
|#
(define (solution board-size knight-position)
  (cond
    ((or (null? board-size) (null? knight-position)) (error "kt-solution arguments must be non-null"))
    ((not (valid-size? board-size)) (raise-argument-error 'kt-solution "board-size doesn't meet the requirements" board-size))
    ((not (valid-position? knight-position board-size )) (raise-argument-error 'kt-solution "position doesn't meet the requirements" knight-position))
    ((not (tour? knight-position board-size)) (displayln "A complete Knight's Tour didn't not exist from this board position! 🐴\n"))
    (else (create-solution board-size knight-position (create-graph board-size)))
  )
)


#|
  
  @param n 
  @param board-size 
  @param knight-position 
  @return 
|#
(define (solutions n board-size knight-position)
  (cond
    ((or (null? board-size) (null? knight-position)) (error "kt-solutions arguments must be non-null"))
    ((or (not (exact-positive-integer? n)) (> n (expt board-size 2))) (raise-argument-error 'kt-solutions "n doesn't meet the requirements" n))
    ((not (valid-size? board-size)) (raise-argument-error 'kt-solutions "board-size doesn't meet the requirements" board-size))
    ((not (valid-position? knight-position board-size )) (raise-argument-error 'kt-solutions "position doesn't meet the requirements" knight-position))
    ((not (tour? knight-position board-size)) (displayln "A complete Knight's Tour didn't not exist from this board position! 🐴\n"))
    (else (create-solutions n board-size knight-position (create-graph board-size)))
  )
)


#|
  
  @param board-size 
  @param solution 
  @return 
|#
(define (test board-size solution)
  (cond 
    ((or (null? board-size) (null? solution)) (error "kt-test arguments must be non-null"))
    ((not (valid-size? board-size)) (raise-argument-error 'kt-test "board-size doesn't meet the requirements" board-size))
    ((not (valid-solution? board-size solution)) (raise-argument-error 'kt-test "solution doesn't meet the requirements" solution))
    (else (print-board (generate-board board-size solution)))
  )
)


#|
  
  @param board-size 
  @param solution 
  @return 
|#
(define (paint board-size solution)
  (cond
    ((or (null? board-size) (null? solution)) (error "kt-paint arguments must be non-null"))
    ((not (valid-size? board-size)) (raise-argument-error 'kt-paint "board-size doesn't meet the requirements" board-size))
    ((not (valid-solution? board-size solution)) (raise-argument-error 'kt-paint "solution doesn't meet the requirements" solution))
    (else (visualizer board-size solution (generate-board board-size solution)))
  )
)


; TEST ----------------------------------------------------------------------------------------------------------------------------

#|
       0  1  2  3  4
  0  (00 00 00 00 00)
  1  (00 00 00 00 00)
  2  (00 00 00 00 00)
  3  (00 00 00 00 00)
  4  (00 00 00 00 00)
|#

(define board-size 5)
(define knight-position '(0 0))
(define n-sol (expt board-size 2))

; (define graph 
;   '(
;     ((0 0) ((1 2) (2 1))) 
;     ((0 1) ((1 3) (2 0) (2 2))) 
;     ((0 2) ((1 0) (1 4) (2 1) (2 3))) 
;     ((0 3) ((1 1) (2 2) (2 4))) 
;     ((0 4) ((1 2) (2 3))) 

;     ((1 0) ((0 2) (2 2) (3 1))) 
;     ((1 1) ((0 3) (2 3) (3 0) (3 2))) 
;     ((1 2) ((0 0) (0 4) (2 0) (2 4) (3 1) (3 3))) 
;     ((1 3) ((0 1) (2 1) (3 2) (3 4))) 
;     ((1 4) ((0 2) (2 2) (3 3))) 

;     ((2 0) ((0 1) (1 2) (3 2) (4 1))) 
;     ((2 1) ((0 0) (0 2) (1 3) (3 3) (4 0) (4 2))) 
;     ((2 2) ((0 1) (0 3) (1 0) (1 4) (3 0) (3 4) (4 1) (4 3))) 
;     ((2 3) ((0 2) (0 4) (1 1) (3 1) (4 2) (4 4))) 
;     ((2 4) ((0 3) (1 2) (3 2) (4 3))) 

;     ((3 0) ((1 1) (2 2) (4 2))) 
;     ((3 1) ((1 0) (1 2) (2 3) (4 3))) 
;     ((3 2) ((1 1) (1 3) (2 0) (2 4) (4 0) (4 4))) 
;     ((3 3) ((1 2) (1 4) (2 1) (4 1))) 
;     ((3 4) ((1 3) (2 2) (4 2))) 

;     ((4 0) ((2 1) (3 2))) 
;     ((4 1) ((2 0) (2 2) (3 3))) 
;     ((4 2) ((2 1) (2 3) (3 0) (3 4))) 
;     ((4 3) ((2 2) (2 4) (3 1))) 
;     ((4 4) ((2 3) (3 2))) 
;   )
; )

; (define sols
;   '(
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (1 3) (0 1) (2 0) (4 1) (3 3) (1 4) (2 2)) 
;     ((0 0) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4) (1 2) (3 1) (1 0) (0 2) (1 4) (3 3) (4 1) (2 0) (0 1) (1 3) (3 4) (4 2) (3 0) (1 1) (0 3) (2 2) (4 3) (2 4)) 
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (3 3) (1 4) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (1 3) (0 1) (2 2) (4 1) (2 0)) 
;     ((0 0) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4) (1 2) (3 3) (1 4) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (1 3) (0 1) (2 0) (4 1) (2 2)) 
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (0 2) (1 4) (3 3) (4 1) (2 0) (0 1) (1 3) (3 4) (4 2) (3 0) (1 1) (0 3) (2 4) (4 3) (3 1) (1 0) (2 2)) 

;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (4 2) (3 0) (1 1) (0 3) (2 4) (4 3) (3 1) (1 0) (0 2) (1 4) (3 3) (4 1) (2 0) (0 1) (2 2) (3 4) (1 3)) 
;     ((0 0) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4) (1 2) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (1 3) (0 1) (2 0) (4 1) (3 3) (1 4) (0 2) (1 0) (2 2)) 
;     ((0 0) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4) (1 2) (2 0) (4 1) (3 3) (1 4) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (1 3) (0 1) (2 2)) 
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (3 3) (1 4) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (1 3) (0 1) (2 0) (4 1) (2 2)) 
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (1 3) (0 1) (2 0) (4 1) (3 3) (1 4) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (2 2) (3 4) (4 2)) 

;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (3 3) (4 1) (2 0) (0 1) (1 3) (3 4) (4 2) (3 0) (1 1) (0 3) (2 4) (4 3) (3 1) (1 0) (2 2) (1 4) (0 2)) 
;     ((0 0) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4) (1 2) (3 1) (1 0) (0 2) (1 4) (3 3) (4 1) (2 0) (0 1) (1 3) (3 4) (4 2) (3 0) (1 1) (0 3) (2 4) (4 3) (2 2)) 
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (4 2) (3 4) (1 3) (0 1) (2 0) (4 1) (3 3) (1 4) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (2 2)) 
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (1 3) (3 4) (4 2) (3 0) (1 1) (0 3) (2 4) (4 3) (3 1) (1 0) (0 2) (1 4) (3 3) (4 1) (2 0) (0 1) (2 2)) 
;     ((0 0) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4) (1 2) (3 3) (1 4) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (1 3) (0 1) (2 2) (4 1) (2 0)) 

;     ((0 0) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4) (1 2) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (1 3) (0 1) (2 0) (4 1) (3 3) (1 4) (0 2) (1 0) (2 2) (4 3) (3 1)) 
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (1 3) (0 1) (2 0) (4 1) (2 2) (1 4) (3 3)) 
;     ((0 0) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4) (1 2) (2 4) (4 3) (3 1) (1 0) (0 2) (1 4) (3 3) (4 1) (2 0) (0 1) (1 3) (3 4) (4 2) (3 0) (2 2) (0 3) (1 1)) 
;     ((0 0) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4) (1 2) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (1 3) (0 1) (2 0) (4 1) (3 3) (1 4) (2 2) (1 0) (0 2)) 
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (1 3) (3 4) (4 2) (3 0) (1 1) (0 3) (2 4) (4 3) (3 1) (1 0) (0 2) (1 4) (3 3) (4 1) (2 2) (0 1) (2 0)) 

;     ((0 0) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4) (1 2) (2 0) (0 1) (1 3) (3 4) (4 2) (3 0) (1 1) (0 3) (2 4) (4 3) (3 1) (1 0) (0 2) (1 4) (2 2) (4 1) (3 3)) 
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (4 2) (3 4) (1 3) (0 1) (2 0) (4 1) (3 3) (1 4) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (2 2) (3 0) (1 1)) 
;     ((0 0) (2 1) (4 0) (3 2) (4 4) (2 3) (0 4) (1 2) (2 0) (4 1) (3 3) (1 4) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (2 2) (0 1) (1 3)) 
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (3 3) (4 1) (2 0) (0 1) (1 3) (3 4) (4 2) (3 0) (1 1) (0 3) (2 4) (4 3) (3 1) (1 0) (0 2) (1 4) (2 2)) 
;     ((0 0) (1 2) (0 4) (2 3) (4 4) (3 2) (4 0) (2 1) (1 3) (0 1) (2 0) (4 1) (3 3) (1 4) (0 2) (1 0) (3 1) (4 3) (2 4) (0 3) (1 1) (3 0) (4 2) (3 4) (2 2)) 
;   )
; )


(displayln "\n>>> KT-Solution 💡 <<<\n")
(solution board-size knight-position)
(displayln "\n")

(displayln "\n>>> KT-Solutions 📦 <<<\n")
(solutions n-sol board-size knight-position)
(displayln "\n")

(displayln "\n>>> KT-Test ✅ <<<\n")
(test board-size (solution board-size knight-position))
(displayln "\n")

(displayln "\n>>> KT-Paint 🎨 <<<\n")
(paint board-size (solution board-size knight-position))
(displayln "\n")