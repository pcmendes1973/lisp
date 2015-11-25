;;;; Solution for Problem 55 in Project Euler
;;;; Paulo Mendes, 24-NOV-2015


(defun digits (n &key (base 10)
                      (append Nil))
"Returns the digits of n in a list
 Keyword 'append' is used solely for recursion"
  (if (< n base)
    (cons n append)
   (multiple-value-bind (div mod)
     (floor n base)
     (digits div :base base
                 :append (cons mod append)))))

(defun is-palindrome? (lst)
"T is lst is a palindrome, Nil otherwise"
  (if (> (length lst) 1)
    (and (= (car lst) (car (last lst)))
         (is-palindrome? (cdr (butlast lst))))
   T))

(defun lychrel-iteration (n)
"Returns n + (palindrome n)"
   (+ n (reduce #'(lambda (x y) (+ (* x 10) y)) (reverse (digits n)))))

(defun iterations-to-palindrome (n)
"Returns the number of iterations to reach a palindrome of n.
 Attempts 50 iterations and returns 51 if no palindrome is found"
   (loop for i from 1 to 51
         for num = (lychrel-iteration n) then (lychrel-iteration num)
         until (is-palindrome? (digits num))
         finally (return i)))

(loop for i from 10 to 9999 counting (> (iterations-to-palindrome i) 50))
