;;;; Solution for Problem 160 in Project Euler
;;;; Paulo Mendes, 4-JAN-2016

(defun factorial-exponent (pn prime)
"Returns the exponent associated with 'prime' in pn!"
  (do ((n (floor pn prime) (floor n prime))
       (s 0 (+ s n)))
      ((= n 0) s)))

(defun eratosthenes (n)
"Calculates all primes smaller than n using the sieve of Eratosthenes"
   (let ((sieve (make-array (1- n) :initial-contents (loop for i from 2 to n collect i))))
      (loop for i across sieve
            if i do (loop for j from (expt i 2) to n by i
                          do (setf (aref sieve (- j 2)) Nil)))
    (values (remove-if #'null sieve))))


(defun exponent-modulo (n exponent modulo)
"Calculates n^exponent mod modulo"
   (case exponent
      (0 1)
      (1 (mod n modulo))
      (otherwise
         (multiple-value-bind (div mod)
            (floor exponent 2)
               (mod (* (expt (exponent-modulo n div modulo) 2)
                       (if (= mod 1) n 1)) modulo)))))

(defun calculate-factorial (lst modulo)
"Calculates a factorial from an a-list containing bases and exponents
 generated per the prime swing algorithm"
   (reduce #'(lambda (x y) (mod (* x y) modulo))
     (mapcar #'(lambda (x) (exponent-modulo (car x) (cdr x) modulo)) lst)))


(defun prime-swing-factorial (n modulo)
"Calculates n! using the prime swing algorithm.
 Outputs the result followed by the number of zeros"
   (let ((zeros))
   (loop for i across (eratosthenes n)
         collect (cons i (factorial-exponent n i)) into factors
         finally (progn
                    (shiftf zeros (cdaddr factors) 0)
                    (decf (cdar factors) zeros)
                    (return (values (calculate-factorial factors modulo)
                                     zeros))))))
                                     
;;;; Another solution for Problem 160
;;;; Uses functions 'factorial-exponent' and 'exponent-modulo' above
;;;; Paulo Mendes, 4-JAN-2016

(defmacro do-primes (var n &rest body)
"Iterates a piece of code over the first n prime numbers. The variable var is set to each
 prime in succession."
   (let ((i (gensym)))
  `(let* ((biggest-prime ,n)
          (array-size (- biggest-prime 1))
          (candidates (make-array array-size :initial-element t)))
      (loop for ,i across candidates
	        for ,var from 2
	        if ,i do (progn 
			     (loop for k from (- (expt ,var 2) 2) below array-size by ,var
			           do (setf (aref candidates k) Nil))
				         (progn ,@body))))))

(defun prime-swing-factorial (n modulo)
"Calculates n! mod modulo using do-primes. Returns n! followed by the number of zeros."
   (let*((exp^2 (factorial-exponent n 2))
         (exp^3 (factorial-exponent n 3))
		 (exp^5 (factorial-exponent n 5))
         (opt (* (exponent-modulo 2 (- exp^2 exp^5) modulo)
		         (exponent-modulo 3 exp^3 modulo))))
      (do-primes p n (if (> p 5)
                        (setf opt (mod (* opt (exponent-modulo p (factorial-exponent n p) modulo)) modulo))))
	  (values opt exp^5)))
