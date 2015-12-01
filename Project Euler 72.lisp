
;;;; Solution for Problem 72 in Project Euler
;;;; Paulo Mendes, 1-DEC-2015

(defun smallest-divisor (n)
   (if (zerop (mod n 2)) 2
     (loop for i from 2 to (isqrt n)
           if (zerop (mod n i))
              do (return-from smallest-divisor i)))
     (values n))

(defun factor (n)
   (let ((sf (smallest-divisor n)))
     (if (= sf n) `(,n)
       (cons sf (factor (/ n sf))))))

(defun totient (n)
   (let ((table (make-hash-table :size 10)))
      (loop for i in (factor n)
            do (setf (gethash i table) 
                 (if (gethash i table)
                (1+ (gethash i table)) 1)))
   (apply #'*
   (loop for base being the hash-keys of table
         using (hash-value exponent)
         collect (* (1- base) (expt base (1- exponent)))))))

(loop for i from 2 to 1000000 summing (totient n))
