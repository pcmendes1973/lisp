;;;; Solution to Problem 16 from Project Euler 
;;;; https://projecteuler.net/problem=16
;;;; Paulo Mendes, 8-OCT-2015


(setq num (expt 2 1000))

(reduce #'+ (loop while (> num 0)
                 collect (mod num 10)
                 do (setf num (floor num 10))))
