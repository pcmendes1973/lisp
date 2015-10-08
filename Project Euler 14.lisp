;;;; Solution for problem 14 in Project Euler
;;;; https://projecteuler.net/problem=14
;;;; Paulo Mendes, 8-OCT-2015

(defparameter *collatz* (make-hash-table))
(setf (gethash 1 *collatz*) 0)

(defun n-collatz (n)
"""Returns the number of steps to reach 1 following the Collatz sequence.
    Memo-ized using hash table *collatz*
    Entry 1 -> 0 serves as stopping criterion for recursive function"""
   (if (gethash n *collatz*) (gethash n *collatz*)
      (setf (gethash n *collatz*)
         (if (evenp n)
            (1+ (n-collatz (/ n 2)))
            (1+ (n-collatz (1+ (* n 3))))))))
            
;;; Calculates the maximum value of n-collatz in interval [1, 1000000]            
(loop for i from 1 to 1000000 maximize (n-collatz i))
