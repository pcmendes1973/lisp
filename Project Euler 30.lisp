;;;; Solution for problem 30 on Project Euler
;;;; https://projecteuler.net/problem=30
;;;; Paulo Mendes, 7-NOV-2015


(defun digits (n &optional (base 10))
"Returns the digits of n in a list"
  (if (< n base)
    (list n)
   (multiple-value-bind (div mod)
      (floor n base)
      `(,@(digits div base) ,mod))))


(defun equals-sum-of-pow? (n &optional (power 4))
"Returns T if n equals sum of the power-th power of digits, Nil otherwise"
   (= 0 (reduce #'(lambda (x y) (- x (expt y power))) (digits n)
                 :initial-value n)))

(time (loop for i from 2 to (* 6 (expt 9 5)) ; Run until upper bound
   when (equals-sum-of-pow? i 5) summing i))
