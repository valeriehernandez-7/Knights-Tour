#lang racket

(require racket/gui/base)

(provide visualizer)

#|
  Sebastián M. Chen Cerdas (https://github.com/seballoll)
  Valerie M. Hernández Fernández (https://github.com/valeriehernandez-7)
  Óscar M. Soto Varela (https://github.com/CAMANEM)

  Knight's Tour (GUI)
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

(define (visualizer board-size solution board)
  (displayln "\nOpening the Knight's Tour 🐴 Visualizer...")
  (displayln "\n>>> KT-Visualizer 💻 <<<")
  (display "'board-size'\t: ")(displayln board-size)
  (display "'solution'\t: ")(displayln solution)
  (display "'board'\t\t: ")(displayln board)(display "\n")
  )


; Load images
(define addButtonIcon (make-object bitmap% "add_button.png"))
;positions to write on chessboard
(define positions '())


;-------------------------Colors and textures-------------------------;

(define no-pen (new pen% [style 'transparent]))
(define whtWood-brush (new brush% [color "white"]))
(define text-brush (new brush% [color "white"]))
(define drkWood-brush (new brush% [color "black"]))
(define wllppr-brush (new brush% [color "black"]))
(define horse-brush (new brush% [color "black"]))
(send wllppr-brush set-stipple (read-bitmap "../resources/horseWallpaper.jpeg"))
(send horse-brush set-stipple (read-bitmap "../resources/horseVector.png"))
(send whtWood-brush set-stipple (read-bitmap "../resources/whtWood.png"))
(send drkWood-brush set-stipple (read-bitmap "../resources/drkWood.png"))
;(define target (make-bitmap 30 30)) ; A 30x30 bitmap
;(define dc (new bitmap-dc% [bitmap target]))

;-------------------------Program window-------------------------;

; Make a frame for the menu
#|
    Creates the window that contains everything
|#
(define mainWindow
  (new frame%
       [label "Knight's Tour 🐴"]
       [width 1280]
       [height 950]
       [style '(no-resize-border)]
       )
  )

;-------------------------Auxiliares-------------------------;


#|
    Quicksort adapted for a list with triplets.
    @param lst: is a disordered list. Ej. '((1 50 50) (3 150 50) (2 100 50))
    @return ordered list. Ex. '((1 50 50) (2 100 50) (3 150 50))
|#
(define (quicksort lst)
  (cond
    ((null? lst) lst)
    (else
     (append
      (quicksort (filter-less (cdr lst) (car lst)))
      (list (car lst))
      (quicksort (filter-greater (cdr lst) (car lst)))
      )
     )
    )
  )

#|
    Quicksort auxiliary, order the numbers that are lower than the pivot.
    @param lst: is a disordered list.
    @param pivot: element of the list selected to compare and order the list.
    @return ordered list.
|#
(define (filter-less lst pivot)
  (cond
    ((null? lst) lst)
    ((< (caar lst) (car pivot)) (append (list (car lst)) (filter-less (cdr lst) pivot)))
    (else (filter-less (cdr lst) pivot))
    )
  )

#|
    Quicksort auxiliary, order the numbers that are higher than the pivot.
    @param lst: is a disordered list.
    @param pivot: element of the list selected to compare and order the list.
    @return ordered list.
|#
(define (filter-greater lst pivot)
  (cond
    ((null? lst) lst)
    ((>= (caar lst) (car pivot)) (append (list (car lst)) (filter-greater (cdr lst) pivot)))
    (else (filter-greater (cdr lst) pivot))
    )
  )


#|
    Gets a specified row of a matrix.
    @param lst: list/matrix to get the row from.
    @param row: specified number of the row to get.
    @param i: is used a counter to move in the matrix.
    @return the specified row.
|#
(define (getRow lst row i)
  (cond
    ((equal? row i) (car lst))
    (else (getRow (cdr lst) row (+ i 1)))
    )
  )

#|
    Gets the specified value in a certain position of a row.
    @param lst: it is a list containing a row.
    @param column: is the position specified to get the value from.
    @param j: is used a counter to move in the list.
    @return the value in the specified position.
|#
(define (getColumn lst column j)
  (cond
    ((equal? column j) (car lst))
    (else (getColumn (cdr lst) column (+ j 1)))
    )
  )


#|
    Gets the value of a specified position in a matrix.
    @param lst: list/matrix to get the value from.
    @param row: specified row to get the value from.
    @param column: specified row to get the value from.
    @param i: is used a counter to move in the matrix.
    @param j: is used a counter to move in the list.
    @return the value in the specified position.
|#
(define (getValueAux lst row column i j)
  (cond
    ((null? lst) lst)
    (else (getColumn (getRow lst row 0) column 0))
    )
  )

#|
    Gets the value of a specified position in a matrix.
    @param lst: list/matrix to get the value from.
    @param row: specified row to get the value from.
    @param column: specified row to get the value from.
    @return the value in the specified position.
|#
(define (getValue lst row column)
  (getValueAux lst row column 0 0)
  )


;-------------------------Canvas-------------------------;

#|
    Creates a list of triplets with the moves of the solution and the coordinates to write them.
    @param size: it is the size of the matrix
    @param movs: it is a matrix with the solution of the horse problem.
    @return a list with the triplets of a solution. Ex. '((1 50 50) (2 100 50))
    the first element of the triplet represents the number if the movement, the second and third
    numbers are the coordinates x and y to write the number in the chessboard.
|#
(define (buildMovLst size movs)
  (for ([i (in-range 0 size)])
    (for ([j (in-range 0 size)])
      (set! positions (append positions (list (list (getValue movs i j) (+ 50 (* j 50)) (+ 15 (* i 50)) ))))
      )
    )
  )

#|
    Draws the canvas background
|#
(define (drawBackground)
  (send dc set-brush wllppr-brush)
  (send dc draw-rectangle 0 0 1280 1000)
  )

#|
    Draws an empty chessBoard of the size n*n
    @param size: n size if the chessboard
|#
(define (drawChessBoard size)
  (send dc set-pen "black" 1 'solid)
  (send dc set-brush whtWood-brush)
  (define wht_color #t)
  ;Draws the chessboard
  (for ([i (in-range 0 size)])
    ;draws each square in a row
    (for ([j (in-range 0 size)])
      (send dc draw-rectangle (+ 50 (* j 50)) (+ 15 (* i 50)) 50 50)
      ;alternate square colors
      (cond
        ((false? wht_color)
         (send dc set-brush whtWood-brush)
         (set! wht_color #t))
        (else
         (send dc set-brush drkWood-brush)
         (set! wht_color #f))
        )
      )
    ;if n is odd, change square color at the end of a row
    (cond
      ((even? size)
       (cond
         ((false? wht_color)
          (send dc set-brush whtWood-brush)
          (set! wht_color #t))
         (else
          (send dc set-brush drkWood-brush)
          (set! wht_color #f) )
         )
       )
      )
    )
  )


#|
    Draws a given number in the given coordinates.
    @param mov: it is a list containing number, position_x and position_y. Ex. '(1 50 50)
|#
(define (drawNumber mov)
  (send dc set-text-foreground "white")
  (send dc draw-text (~v (car mov) ) (cadr mov) (caddr mov) #f 0 0)
  ;(send dc set-brush horse-brush)
  ;(send dc set-scale 1 0.9)
  ;(send dc draw-bitmap (bitmap-scale selectBackground 0.25) 0 0)
  ;(send dc draw-rectangle (cadr mov) (caddr mov) 45 50)
  )


#|
    Creates a panel for buttons and controls.
|#
(define controlPanel
  (new horizontal-panel%
       [parent mainWindow]
       [border 0]
       [spacing 0]
       [alignment '(center center)]
       )
  )

#|
    Creates a panel for the chessBoard.
|#
(define chessPanel
  (new horizontal-panel% [parent controlPanel]
       [border 0]
       [spacing 0]
       [min-width 0]
       [alignment '(center center)]
       )
  )


; Draws the next movement of the horse in the chessBoard.
(define (drawNext)
  (cond
    ((null? positions) (send nextBtn enable #f))
    (else
     (drawNumber (car positions))
     (set! positions (cdr positions)))
    )
  )


; Creates a button to show the next horse movement.
(define nextBtn
  (new button%
       [parent controlPanel]
       [label "Next"]
       [callback (lambda (button event)(drawNext))]
       )
  )

#|
    Creates a canvas to draw the chessBoard.
|#
(define chessBoard
  (new canvas%
       [parent chessPanel]
       [paint-callback
        (lambda (canvas dc)
          (drawBackground)
          (drawChessBoard 5))]
       )
  )

; Makes accesible the dc, to be able to draw from other functions.
(define dc (send chessBoard get-dc))


; ; Creates a menu bar
; (define menu-bar (new menu-bar%
;                       (parent mainWindow)
;                       )
;   )
; (define menu (new menu%
;                   (label "&Next")
;                   (parent menu-bar)
;                   )
;                   )

; Show the frame by calling its show method
(send mainWindow show #t)

;sleep solves a drawing problem after draw canva
(sleep/yield 0.01)

;Declares a base problem with an 5*5 size
(buildMovLst
 5
 '(
   (01 06 15 10 21)
   (14 09 20 05 16)
   (19 02 07 22 11)
   (08 13 24 17 04)
   (25 18 03 12 23)
   )
 )

;Creates an ordered list with the movements and coordinates of the solution to draw easily
(set! positions (quicksort positions))
