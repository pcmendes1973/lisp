;;;; Returns all change permutations for amount given coin values in parameter coins
;;;; Problem from Abelson & Sussman
;;;; Paulo Mendes, 22/JUL/2015


(defparameter *coins* '(50 25 10 5 1))

(defun change (amount coins)
"Returns all change permutations for amount given coin values in parameter coins"
  (cond ((or (null coins) (< amount 0)) Nil)
        ((eq amount 0) (list Nil))
        ((null (cdr coins)) (list (make-list (floor amount (car coins)) :initial-element (car coins))))
        (t (append (change amount (cdr coins))
                   (loop for i in (change (- amount (car coins)) coins) collect (cons (car coins) i))))))
