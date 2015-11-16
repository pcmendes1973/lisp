;;;; Project Euler Problems 1 and 2
;;;; https://projecteuler.net/problem=1 and https://projecteuler.net/problem=2
;;;; Paulo Mendes, 10-OCT-2015

;;; Problem 1
(loop for i from 1 to 999
    summing (if (or (= (mod i 3) 0) (= (mod i 5) 0)) i 0))

;;; Problem 2
(loop for i = 0 then j
      and j = 1 then (+ i j)
      until (> j 4000000)
      summing (if (evenp j) j 0))

;;; Problem 3
;;; 12-NOV-2015
(defun divisors (n)
"Returns all prime divisors of n in ascending order"
  (let ((lowest-divisor (loop for i from 2 to (isqrt n)
                              when (zerop (mod n i)) return i)))
  (if lowest-divisor
     (cons lowest-divisor (divisors (/ n lowest-divisor)))
   (list n))))
