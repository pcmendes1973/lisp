;;;; Functions for calculating polynomials
;;;; Loosely based in problems from Abelson & Sussman
;;;; Paulo Mendes, 22/JUL/2015 - 25/JUL/2015

(defun polyn (lst x)
  "Calculates the value of the polynomial defined by list lst for value x (recursive version)"
  (if (cdr lst)
    (+ (car lst) (* x (polyn (cdr lst) x)))
    (car lst)))

(defun polyn (lst x)
  "Calculates the value of the polynomial defined by list lst for value x (loop version)"
  (let ((s 0))
    (loop for i in lst do (setf s (+ (* x s) i)))
    (values s)))

(defun polyn (lst x)
  "Calculates the value of the polynomial defined by list lst for value x--the correct way!"
  (reduce #'(lambda (a b) (+ (* a x) b)) lst))
