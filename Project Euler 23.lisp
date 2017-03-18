;;;; Solution for Problem 23 in Project Euler
;;;; Paulo Mendes, 17-MAR-2017

(defun divisors (n1 n2)
"Returns the divisors of the product between n1 and n2, 
 which can be either prime numbers or lists of divisors."
   (sort (remove-duplicates
            (mapcan #'(lambda (y) 
               (mapcar #'(lambda (x) (* x y))
                       (if (numberp n1) (list 1 n1) n1)))
                    (if (numberp n2) (list 1 n2) n2)))
         #'<))

(let ((sieve (make-array 28122 :initial-contents (loop for i from 2 to 28123 collect i))))
      (loop for i across sieve
            if i do (loop for j from (expt i 2) to 28123 by i
                          do (setf (aref sieve (- j 2)) Nil)))
    (defun find-smallest-prime-divisor (n)
        (loop for i across sieve
              when (and i (numberp i) (zerop (mod n i)))
                return i))
(let ((abundant (loop for i across sieve
                      for n from 2
                      for d = (when (null i)
                                (find-smallest-prime-divisor n))
                      when d do (setf (aref sieve (- n 2))
                                            (divisors (aref sieve (- (/ n d) 2)) d))
                      when (and (listp (aref sieve (- n 2)))
                                             (> (reduce #'+ (butlast (aref sieve (- n 2)))) n))
                         collect n))
      (sum-of-abundant (make-array 28123 :initial-contents (loop for i from 1 to 28123 collect i))))

(loop for i in abundant
      do (loop for j in abundant
               for k = (+ i j) 
               until (> k 28123) do
                  (setf (aref sum-of-abundant (1- k)) 0)))
       (values (reduce #'+ sum-of-abundant))))
