;;;; Project Euler Problem 40
;;;; https://projecteuler.net/problem=40
;;;; Paulo Mendes, 13-OCT-2015

(setq champernowne-digit
(let ((n 0)             ; number from which digits are derived (1, 2, 3,...)
      (digits '()))     ; digits of n ordered in a list

(defun get-digits (n)
"Returns a list containing the digits of integer n in order"
   (loop for i = n then (car j)
         for j = (multiple-value-bind (div mod) (floor i 10) (list div mod))
         collect (cadr j) into l
         while (> (car j) 0)
         finally (return (nreverse l))))

#'(lambda ()
"Returns a digit of the decimal portion of Champernowne's constant each time it's called"
   (or digits (setf digits (get-digits (incf n))))   ; Make sure there are digits to play with and increments n as needed
    (values (car digits) (setf digits (cdr digits))))))

(reduce #'*                        ; Gets the requested numbers and multiplies them
  (loop for i from 1 to 1000000
        for j = (funcall champernowne-digit)
        if (find i '(1 10 100 1000 10000 100000 1000000)) collect j))
