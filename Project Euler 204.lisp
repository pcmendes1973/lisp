
;;;; Solution for Problem 204 in Project Euler
;;;; Paulo Mendes, 16-SEP-2017

(defconstant *primes* '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))

(defparameter *limit* (expt 10 9))


(defun powers (n limit)
   (loop for i = 1 then (* n i)
         until (> i limit) collect i))

(defun cartesian-product (list1 list2 &optional (prune #'(lambda (x) (< *limit* x))))
   (loop for i in list1
         nconc (loop for j in list2
                     for var = (* i j)
                     unless (funcall prune var) collect var)))
                  
(length (reduce #'cartesian-product (loop for i in *primes* collect (powers i *limit*))))
