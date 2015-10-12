;;;; Solution for problem 9 in Project Euler
;;;; https://projecteuler.net/problem=9
;;;; Paulo Mendes, 11-OCT-2015

;;; This problem can be solved without programming by finding a primitive
;;; pythagorean triplet where the sum of the numbers is a divisor of
;;; 1,000; that triplet is 8, 15, and 17


;; Primitive Pythagorean triplet where a + b + c is a divisor of 1000
(defparameter *pythagorean-triplet* '(8 15 17))

(loop for i in *pythagorean-triplet*
      for j = (* 25 i)
      summing j into proof-sum
      collect j into vals
      finally (return
(format t "~{~a~#[~;^2 = ~:;^2 + ~]~}^2~%~
           ~:*~{~a~^ + ~} = ~d~%~
           ~:*~:*~{~a~^ * ~} = ~*~a" vals proof-sum (reduce #'* vals))))
