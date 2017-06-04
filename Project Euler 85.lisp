;;;; Solution for Problem 85 in Project Euler
;;;; Paulo Mendes, 02-JUN-2017


(defun m (p q n)
   (labels ((find-m (&optional (a p) (b q))
                (let ((mul (* a b)))
                  (if (> mul n) 0
                    (max mul (find-m (* a p) b) (find-m a (* b q)))))))
                (find-m)))

(defun eratosthenes (n)
"Finds all primes smaller than n using the sieve of Eratosthenes"
   (let ((sieve (make-array (1- n) :initial-contents (loop for i from 2 to n collect i))))
      (loop for i across sieve
            if i do (loop for j from (expt i 2) to n by i
                          do (setf (aref sieve (- j 2)) Nil)))
    (values (remove-if #'null sieve))))


(defun s (n)
    (loop for p in (mapcon #'list (coerce (eratosthenes (floor n 2)) 'list))
          summing (or (loop for q in (cdr p)
                            summing (m (car p) q n)
                            until (zerop (m (car p) q n))) 0)))
