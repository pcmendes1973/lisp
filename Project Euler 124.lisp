;;;; Solution for Problem 124 in Project Euler
;;;; Paulo Mendes, 25-SEP-2017

(defun radicals (n)
"Returns an array containing the product of the distinct prime
factors of all numbers between 1 and n"
  (loop with array = (make-array n :initial-element 1)
        for i across (make-array (1- n)
                                 :displaced-to array
                                 :displaced-index-offset 1)
        for j from 2
        when (= i 1)
          do (loop for k from (1- j) below n by j
                   do (setf (aref array k) (* j (aref array k))))
        finally (return array)))


(car (nth 9999
  (sort (map 'list #'cons (loop for i from 1 to 100000 collecting i) (radicals 100000))
        #'(lambda (x y) (if (= (cdr x) (cdr y)) 
                           (< (car x) (car y))
                          (< (cdr x) (cdr y)))))))
