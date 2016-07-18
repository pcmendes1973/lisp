;;;; Solution for problem 57 in Project Euler
;;;; Paulo Mendes, 17-JUL-2016

(defun n-digits (n)
"Returns the number of digits in n"
   (length (write-to-string n)))

(loop for i from 1 to 1000
      for x = 1 then (/ (+ (numerator x)
                        (* 2 (denominator x)))
                     (+ (numerator x) (denominator x)))
      counting (> (n-digits (numerator x))
                  (n-digits (denominator x))))
