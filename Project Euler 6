;;;; Solution for problem 6 in Project Euler
;;;; https://projecteuler.net/problem=6
;;;; Paulo Mendes, 13-OCT-2015

(defparameter n 100)

(loop for i from 1 to n
      summing (* i i) into squares
      finally (return (- (expt (/ (* n (1+ n)) 2) 2) squares)))
