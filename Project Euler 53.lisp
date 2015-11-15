;;;; Solution for Problem 53 in Project Euler
;;;; https://projecteuler.net/problem=53
;;;; Paulo Mendes, 15-NOV-2015

(loop for i from 3 to 100
      for j = '(1 3 3 1) then `(1 ,@(mapcar #'+ j (cdr j)) 1)
      summing (count-if #'(lambda (x) (> x 1000000)) j))
