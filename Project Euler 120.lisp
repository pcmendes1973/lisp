;;;; Solution for Problem 120 in Project Euler
;;;; Paulo Mendes, 6-JUN-2017

(loop for a from 3 to 1000
      summing (* 2 a (floor (1- a) 2)))
