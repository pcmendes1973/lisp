;;;; Solution to Problem 112 in Project Euler
;;;; Paulo Mendes, 21-NOV-2015



(defun digits (n &optional (base 10))
"Returns the digits of n in a list"
  (if (< n base)
    (list n)
   (multiple-value-bind (div mod)
      (floor n base)
      `(,@(digits div base) ,mod))))


(defun is-bouncy? (n)
"T if n is bouncy, Nil otherwise"
   (let ((digits-of-n (digits n)))
       (not (or (apply #'>= digits-of-n)
                (apply #'<= digits-of-n)))))

(loop for i from 99 
      count (is-bouncy? i) into bouncy
      until (= (/ bouncy i) 99/100)
      finally (return i))
