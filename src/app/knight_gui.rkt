#lang racket

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