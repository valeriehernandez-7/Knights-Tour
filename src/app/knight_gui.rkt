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


(define ui-board-size null)
(define ui-board null)
(define ui-board-clip null)
(define ui-solution null)
(define horse-i 0)
(define horse-j 0)


;-------------------------Colors and textures-------------------------;

(define whtWood-brush (new brush% [color "black"]))
(define drkWood-brush (new brush% [color "black"]))
(define wllppr-brush (new brush% [color "black"]))
(send wllppr-brush set-stipple (read-bitmap "../resources/horseWallpaper.jpeg"))
(send whtWood-brush set-stipple (read-bitmap "../resources/whtWood.png"))
(send drkWood-brush set-stipple (read-bitmap "../resources/drkWood.png"))

;-------------------------Program window-------------------------;

; Creates the window that contains everything
(define mainWindow 
    (new frame% 
        [label "Knight's Tour 🐴"]
        [width 900]
        [height 915]
        [style '(no-resize-border)]
    )
)

;-------------------------Aux-------------------------;

(define dc null)

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
(define (getRow lst row (i 0))
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
(define (getFromColumn lst column (j 0))
    (cond
        ((equal? column j) (car lst))
        (else (getFromColumn (cdr lst) column (+ j 1)))
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
    (cond
        ((null? lst) lst)
        (else (getFromColumn (getRow lst row) column))
    )
)

;-------------------------Canvas-------------------------;

; Draws the canvas background
(define (drawBackground)
    (send dc set-brush wllppr-brush)
    (send dc draw-rectangle 0 0 900 915)
)


#|
    Draws an empty chessBoard of the size n*n
    @param size: size n of the chessBoard.
|#
(define (drawChessBoard size)
    (send dc set-pen "black" 3 'solid)
    (send dc set-brush whtWood-brush)
    (define wht_color #t)
    ;Draws the chessboard
    (for ([i (in-range 0 size)])
        ;draws each square in a row
        (for ([j (in-range 0 size)])
            (send dc draw-rectangle (+ 45 (* j 45)) (+ 15 (* i 45)) 45 45)
            ;alternate square colors
            (cond
                ((false? wht_color)
                    (send dc set-brush whtWood-brush)
                    (set! wht_color #t)
                )
                (else
                    (send dc set-brush drkWood-brush)
                    (set! wht_color #f)
                )
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
    @param mov it is a list containing number, position_x and position_y. Ex. '(1 50 50)
|#
(define (drawNumber mov)
    (send dc set-font (make-object font% 9 "Tahoma" 'default 'normal 'bold))
    (send dc set-text-foreground "white")
    (send dc draw-text (~v (car mov)) (cadr mov) (caddr mov) #f 0 0)
)


#|
    Draws the horse in the given position.
    @param mov it is a list containing number, position_x and position_y. Ex. '(1 50 50)
|#
(define (drawHorse mov)
    (send dc set-font (make-object font% 24 "Tahoma" 'default 'normal 'bold))
    (send dc set-text-foreground "white")
    (set! horse-i (+ (cadr mov) 9))
    (set! horse-j (+ (caddr mov) 11))
    (send chessPanel refresh)
    (sleep/yield 0.01)
)

#|
    Draws the movements required when the slider is moved
    @param n number selected in the slider.
|#
(define (drawSlider n)
    (cond ((< 0 n) (drawHorse (getRow ui-board (- n 1)))))
    (for ([i (in-range 0 n)]) (drawNumber (getRow ui-board i)))
)

; Creates a panel for buttons and controls.
(define controlPanel 
    (new vertical-panel% 
        [parent mainWindow]
        [alignment '(center center)]
    )
)


; Creates a panel for the chessBoard
(define chessPanel 
    (new panel% 
        [parent controlPanel]
        [alignment '(center center)]
    )
)

; Creates a canvas to draw the chessBoard.
(define (chessBoard size) 
    (new canvas% 
        [parent chessPanel]
        [paint-callback (lambda (canvas dc) 
                    (drawBackground)
                    (drawChessBoard size)
                    (send dc set-text-foreground "white")
                    (send dc set-font (make-object font% 24 "Tahoma" 'default 'normal 'bold))
                    (send dc draw-text "♞" horse-i horse-j)
            )
        ]
    )
)


#|
    Creates a list of triplets with the moves of the solution and the coordinates to write them.
    @param size: it is the size of the matrix
    @param movs: it is a matrix with the solution of the horse problem.
    @return a list with the triplets of a solution. Ex. '((1 50 50) (2 100 50))
    the first element of the triplet represents the number if the movement, the second and third
    numbers are the coordinates x and y to write the number in the chessboard.
|#
(define (buildMovLst size movs)
    (cond
        ((null? movs) movs)
        (else
            (for ([i (in-range 0 size)])
                (for ([j (in-range 0 size)])
                    (set! ui-board (append ui-board (list (list (getValue movs i j) (+ 48 (* j 45)) (+ 16 (* i 45))))))
                    (set! ui-board-clip ui-board)
                )
            )
        )
    )
)


(define (play)
    (cond
        ((and (not (null? ui-board-size)) (not (null? ui-board)) (not (null? ui-solution)))
            ; Makes accesible the dc, to be able to draw from other functions.
            (set! dc (send (chessBoard ui-board-size) get-dc))
            ;Creates an ordered list with the movements and coordinates of the solution to draw easily
            (set! ui-board (quicksort ui-board))
            ; animation controller set-up
            (define animation-controller 
                (new slider% 
                    [parent controlPanel]
                    [label #f]
                    [min-value 1]
                    [max-value (expt ui-board-size 2)]
                    [callback (lambda (slider event) (drawSlider (send slider get-value)))]
                )
            )
            (drawSlider (send animation-controller get-value))
            ;sleep solves a drawing problem after draw canva
            (sleep/yield 0.01)
            ; Show the frame by calling its show method
            (send mainWindow show #t)
        )
    )
)


(define (visualizer board-size solution board)
    (displayln "\nOpening the Knight's Tour 🐴 Visualizer...")
    (displayln "\n>>> KT-Visualizer 💻 <<<")
    (display "'board-size'\t: ")(displayln board-size)
    (display "'solution'\t: ")(displayln solution)
    (display "'board'\t\t: ")(displayln board)(display "\n")

    (set! ui-board-size board-size)
    (buildMovLst ui-board-size board)
    (set! ui-solution solution)
    (play)
)