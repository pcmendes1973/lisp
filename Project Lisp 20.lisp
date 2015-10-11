;;;; Solution for problem 20 in Project Euler
;;;; https://projecteuler.net/problem=20
;;;; Paulo Mendes, 11-OCT-2015

(defun factorial (n)
 (loop for i from 1 to n
       for j = 1 then (* i j)
      finally (return j)))

(defun sum-digits (n)
"Sums all the digits of integer n"
(loop for i = n then (car j)
      for j = (multiple-value-bind (div mod) (floor i 10) (list div mod))
      until (= i 0)
      summing (cadr j)))
      
(sum-digits (factorial 100))
