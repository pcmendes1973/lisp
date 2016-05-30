;;;; Solution for problem 20 in Project Euler
;;;; https://projecteuler.net/problem=20
;;;; Paulo Mendes, 11-OCT-2015

(defun factorial (n)
 (loop for i from 1 to n
       for j = 1 then (* i j)
      finally (return j)))

(defun sum-digits (n)
"Sums all the digits of integer n"
  (loop for i = n then (floor i 10) while (> i 0) summing (mod i 10)))
      
(sum-digits (factorial 100))
