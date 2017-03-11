;;;; Solution for Problem 44 in Project Euler
;;;; Paulo Mendes, 11-MAR-2017

(defun is-pentagonal? (n)
"Returns 'n' if n is pentagonal, Nil otherwise"
   (let* ((v (1+ (* 24 n))) (s (isqrt v)))
     (and (= (* s s) v) (= (mod s 6) 5) n)))

(loop for n from 2
      when (loop for m from 1 below n               
                 when (and (is-pentagonal? (/ (+ (* 3 m m) (- m) (* n (1- (* n 3)))) 2))
                           (is-pentagonal? (/ (* (- n m) (+ (* 3 m) (* n 3) -1)) 2)))
                    return it)
         return it)
