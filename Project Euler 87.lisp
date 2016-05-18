;;;; Solution for Probelm 87 in Project Euler
;;;; Paulo Mendes, 18-MAY-2016

(defparameter *max* 50000000)

(defun eratosthenes (n)
"Calculates all primes smaller than n using the sieve of Eratosthenes"
   (let ((sieve (make-array (1- n) :initial-contents (loop for i from 2 to n collect i))))
      (loop for i across sieve
            if i do (loop for j from (expt i 2) to n by i
                          do (setf (aref sieve (- j 2)) Nil)))
    (values (remove-if #'null sieve))))

(defun powers-list (pow)
   (loop for i across (eratosthenes (floor (expt *max* (/ pow))))
         collect (expt i pow) into contents
         finally (return (coerce contents 'vector))))


(setf catches (make-hash-table))

(loop for i across (powers-list 2)
  nconc (loop for j across (powers-list 3)
   nconc (loop for k across (powers-list 4)
               for s = (+ i j k)
               when (< s *max*) do (setf (gethash s catches) t))))
