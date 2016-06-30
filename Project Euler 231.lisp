;;;; Solution for Problem 231 in Project Euler
;;;; Paulo Mendes 30-JUN-2016

(defun eratosthenes (n)
"Calculates all primes smaller than n using the sieve of Eratosthenes"
   (let ((sieve (make-array (1- n) :initial-contents (loop for i from 2 to n collect i))))
      (loop for i across sieve
            if i do (loop for j from (expt i 2) to n by i
                          do (setf (aref sieve (- j 2)) Nil)))
    (values (remove-if #'null sieve))))

(defun factorial-exponent (pn prime)
"Returns the exponent associated with 'prime' in pn! in the prime swing
 factorial algorithm."
  (do ((n (floor pn prime) (floor n prime))
       (s 0 (+ s n)))
      ((= n 0) s)))

(defun sum-terms (n p)
"Sums all divisors of in binom (n, p)"
   (loop for i across (eratosthenes n)
         summing (* i (+ (factorial-exponent n i)
                         (- (factorial-exponent p i))
                         (- (factorial-exponent (- n p) i))))))
