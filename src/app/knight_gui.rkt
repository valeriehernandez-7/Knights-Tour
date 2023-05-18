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

;-------------------------Program window-------------------------;

; Creates the window that contains everything
(define mainWindow 
    (new frame% 
        [label "Knight's Tour 🐴"]
        [width 1280]
        [height 800]
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
                    (set! ui-board (append ui-board (list (list (getValue movs i j) (+ 50 (* j 50)) (+ 15 (* i 50))))))
                    (set! ui-board-clip ui-board)
                )
            )
        )
    )
)


; Draws the canvas background
(define (drawBackground)
    (send dc set-brush wllppr-brush)
    (send dc draw-rectangle 0 0 1280 1000)
)


; Draws an empty chessBoard of the size n*n
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
    @param mov: it is a list containing number, position_x and position_y. Ex. '(1 50 50)
|#
(define (drawNumber mov)
    (send dc set-text-foreground "white")
    ;(drawBackground)
    ;(drawChessBoard size)
    (send dc draw-text (~v (car mov)) (cadr mov) (caddr mov) #f 0 0)

    
)
(define (drawHorse mov)

    (send dc set-text-foreground "white")
    (set! horse-i (cadr mov))
    (set! horse-j (caddr mov))
    (send chessPanel refresh)
    (sleep/yield 0.01)
    
    
)

(define (drawSlider n)
;(send chessPanel refresh)

    (cond ((< 0 n) 
            (drawHorse (getRow ui-board (- n 1))))
        )

    (for ([i (in-range 0 n)])
                    (drawNumber (getRow ui-board i))
                    
                )
    
    
    
    )

; Creates a panel for buttons and controls.
(define controlPanel 
    (new horizontal-panel% 
        [parent mainWindow]
        [border 0]
        [spacing 0]
        [alignment '(center center)]
    )
)



; Creates a panel for the chessBoard
(define chessPanel 
    (new horizontal-panel% 
        [parent controlPanel]
        [border 0]
        [spacing 0]
        [alignment '(center center)]
    )
)



(define (autoDraw)
  (drawHorse (getRow ui-board (- (* ui-board-size ui-board-size) 1)))
  (for ([i (in-range 0 (* ui-board-size ui-board-size))])
                    (drawHorse (getRow ui-board i))
                    (for ([j (in-range 0 i)])
                        (drawNumber (getRow ui-board j))
                        )
                    ;(drawNumber (getRow ui-board i))
                    (sleep 0.5)
                )
  )





; Creates a button to automate the show of the next horse movement.
(define autoBtn
    (new button% 
        [parent mainWindow]
        [label "Auto"]
        [callback (lambda (button event)(thread autoDraw))]
        [callback (lambda (button event)(thread autoDraw))]
    )
)



; Creates a canvas to draw the chessBoard.
(define (chessBoard size) 
    (new canvas% 
        [parent chessPanel]
        [paint-callback (lambda (canvas dc) 
        (drawBackground)
        (drawChessBoard size)
        (define horse-text "♞")
        (send dc set-text-foreground "white")
        (send dc set-font (make-object font% 36 'default))
        (send dc draw-text horse-text horse-i horse-j)
        )]
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

(new slider% 
        [parent mainWindow]
        [label "slider"]
        [min-value 0]
        [max-value 324] ; don´t allow to asign ui-board-size
        [callback (lambda (slider event) 
            (cond ((<= (send slider get-value) (length ui-board)) ; just with lenght let me do the comparison. ui-board-size caused error.
                (drawSlider (send slider get-value) ))
            ))]
    )

(define (play)
    (cond
        ((and (not (null? ui-board-size)) (not (null? ui-board)) (not (null? ui-solution)))
            ; Makes accesible the dc, to be able to draw from other functions.
            (set! dc (send (chessBoard ui-board-size) get-dc))
            ;Creates an ordered list with the movements and coordinates of the solution to draw easily
            (set! ui-board (quicksort ui-board))
            ;sleep solves a drawing problem after draw canva
            (sleep/yield 0.01)
            ; Show the frame by calling its show method
            (send mainWindow show #t)
        )
    )
)
