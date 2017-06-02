;;;; Solution for Problem 117 in Project Euler
;;;; Paulo Mendes, 31-MAY-2017
(do ((i '(8 4 2 1)
        (push (+ (first i) (second i) (third i) (fourth i)) i)))
     ((= (length i) 50) (car i)))
