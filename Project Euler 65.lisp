;;;; Solution for Problem 65 in Project Euler
;;;; Paulo Mendes, 20-MAR-2017

(let ((sum 0))
   (loop for n = (numerator (reduce #'(lambda (x y) (+ y (/ x)))
                                    (reverse *convergents-of-e*)))
             then (multiple-value-bind (q r) (floor n 10)
                    (prog1 q (incf sum r)))
         until (zerop n))
   (values sum))
