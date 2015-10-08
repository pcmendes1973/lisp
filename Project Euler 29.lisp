;;;; Solution to problem 29 in Project Euler
;;;; https://projecteuler.net/problem=29
;;;; Paulo Mendes, 8-OCT-2015


(defparameter *nums* '())

;;; Builds a list *nums* of all a^b where a in [2,100] and b in [2,100]
(loop for i from 2 to 100 do
   (loop for j from 2 to 100 do (setf *nums* (cons (expt i j) *nums*))))

;;; Calculates length after removing duplicates
(length (remove-duplicates *nums*))

;;; Solution in a single line, just for kicks
(length (remove-duplicates (loop for n from 0 to 9701 collect (expt (+ 2 (floor n 98)) (+ 2 (mod n 99))))))
