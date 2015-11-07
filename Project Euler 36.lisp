;;;; Solution for problem 36 in Project Euler
;;;; https://projecteuler.net/problem=36
;;;; Paulo Mendes, 03-NOV-2015

(defun digits (n &optional (base 10))
"Returns the digits of n in a list"
  (if (< n base)
    (list n)
   (multiple-value-bind (div mod)
      (floor n base)
      `(,@(digits div base) ,mod))))

(defun is-palindrome? (lst)
"T is lst is a palindrome, Nil otherwise"
  (if (> (length lst) 1)
    (and (= (car lst) (car (last lst)))
         (is-palindrome? (cdr (butlast lst))))
   T))

;; Sums all numbers having palindrome digits in both base 2 and base 10
(loop for n from 1 to 1000000
      if (and (is-palindrome? (digits n))
              (is-palindrome? (digits n 2))) summing n)
