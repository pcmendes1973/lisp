;;;; Project Euler Problems 1 to 4
;;;; https://projecteuler.net/problem=1, https://projecteuler.net/problem=2
;;;; https://projecteuler.net/problem=3 and https://projecteuler.net/problem=2
;;;; Paulo Mendes, 10-OCT-2015

;;; Problem 1
;;; 10-OCT-2015
(loop for i from 1 to 999
    summing (if (or (= (mod i 3) 0) (= (mod i 5) 0)) i 0))

;;; Problem 2
;;; 10-OCT-2015
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

;;; Problem 4
;;; 11-OCT-2015
(defun is-palindrome (n) ;; Creates a list with the digits and tests whether it's palindromic
"Returns n if it's palindrome, 0 otherwise"
  (let ((digits (loop for i = n then (floor i 10) while (> i 0) collect (mod i 10))))
    (loop for i in digits
          for j in (reverse digits) while (= i j)
          finally (return (if (= i j) n 0)))))
          
(loop for i from 999 downto 1
      until (> result (expt i 2))  ; return when i^2 < result (all subsequent products'll be smaller)
      for j = (loop for k from i downto 1 maximize (is-palindrome (* i k))) 
      maximize j into result       ; find the largest palindrome where i is a multiplier
            finally (return result))
            
            
;;;; END
