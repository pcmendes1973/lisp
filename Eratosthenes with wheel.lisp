;;;; Sieve of Eratosthenes using a factorization wheel
;;;; Paulo Mendes, 6-DEC-2015

(defun ring (&rest content)
"Returns a circular list containing the elements in content.
 The list starts on the first element of content."
   (setf (cdr (last content)) content))

(defun factorization-wheel (lst)
"Returns a circular list containing a factorization wheel for
 a list of prime numbers in lst"
   (let ((circumference (apply #'* lst)))
   (loop for i from 1 to circumference
         unless (some #'(lambda (x) (zerop (mod i x))) lst)
            collect i into wheel
         finally (return (apply #'ring (maplist #'(lambda (x) (if (cdr x)
                         (- (cadr x) (car x))
                        (- circumference (car x) -1))) wheel))))))

(defun eratosthenes (n &optional (wheel (ring 4 2)))
"Returns all integers from up to n, calculated using
 the Sieve of Eratosthenes and a factorization wheel"
   (let* ((candidates (loop with s = 1
                            for i in wheel
                            collect (setf s (+ i s))
                            until (> s n))))
       (maplist #'(lambda (x) (if (> (expt (car x) 2) n)
                                (return-from eratosthenes candidates))
                               (delete-if #'(lambda (y) (zerop (mod y (car x)))) (cdr x))) candidates)))
