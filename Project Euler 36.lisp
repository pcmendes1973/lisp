;;;; Solution for problem 36 in Project Euler
;;;; https://projecteuler.net/problem=36
;;;; Paulo Mendes, 03-NOV-2015

(defun digits (n)
"Returns the digits of n in a list"
  (if (< n 10)
    (list n)
   (multiple-value-bind (div mod)
      (floor n 10)
      (append (digits div) (list mod)))))

(defun to-binary (n)
"Returns the binary digits of n in a list"
   (if (< n 2)
     (list n)
    (append (to-binary (floor n 2)) (list (logand n 1)))))
    
(defun is-palindrome? (lst)
"t is lst is a palindrome, F otherwise"
  (if (> (length lst) 1)
    (and (= (car lst) (car (last lst)))
         (is-palindrome? (cdr (butlast lst))))

;; Sums all numbers having paindrome digits both in base 2 and in base 10
(loop for i from 10 to 1000000
      if (and (is-palindrome? (digits i))
              (is-palindrome? (to-binary i))) summing i)
