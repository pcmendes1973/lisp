;;;; Solution for problem 34 in Project Euler
;;;; https://projecteuler.net/problem=34
;;;; Paulo Mendes, 8-NOV-2015

(let ((factorials #(1 1 2 6 24 120 720 5040 40320 362880)))
(defun factorion (n)
  (if (< n 10)
    (aref factorials n)
   (multiple-value-bind (div mod)
      (floor n 10)
      (+ (aref factorials mod) (factorion div))))))

(loop for i from 10 to 50000
   when (= i (factorion i)) summing i)
