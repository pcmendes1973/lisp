;;;; Solution for problem 4 in Project Euler
;;;; https://projecteuler.net/problem=4
;;;; Paulo Mendes, 11-OCT-2015

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
