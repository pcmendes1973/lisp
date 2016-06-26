;;;; Solution for Problem 47 in Project Euler
;;;; Paulo Mendes, 25-JUN-2016

(defun eratosthenes (n)
"Calculates all primes smaller than n using the sieve of Eratosthenes"
   (let ((sieve (make-array (1- n) :initial-contents (loop for i from 2 to n collect i))))
      (loop for i across sieve
            if i do (loop for j from (expt i 2) to n by i
                          do (setf (aref sieve (- j 2)) Nil)))
    (values (remove-if #'null sieve))))


(defun apply-divisor (n d)
   (loop with v = n
         while (multiple-value-bind (div mod)
                  (floor v d)
                  (let ((divisible (zerop mod)))
                    (when divisible (setf v div))
                    (values divisible)))
         finally (return v)))

(let ((primes (eratosthenes 100000)))
    (defun count-factors (n)
        (loop for i across primes
              counting (if (zerop (mod n i))
                              (if (= 1 (setf n (apply-divisor n i)))
                                  (return (1+ divisors)) t)) into divisors
              finally (return divisors))))

(loop for i from 5
      for counts = '(1 2 1 1) then (cons (count-factors i)
                                                (butlast counts))
      until (equal counts '(4 4 4 4))
      finally (return (- i 3)))
