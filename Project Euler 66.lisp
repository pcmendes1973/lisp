;;;; Solution for Problem 66 in Project Euler
;;;; Paulo Mendes, 18-MAR-2017


(defun continued-fraction (n)
"Returns the continued fraction for the square soot of n.
 Two values are returned: the periodic portion of the fraction
 as a list and the integer square root of n, which corresponds 
 to the initial term."
   (let ((a0 (isqrt n)))
      (do* ((a a0 (floor (/ (+ a0 b) c)))
            (b a0 (- (* a c) b))
            (c (- n (* a a)) (/ (- n (* b b)) c))
            (lst (list a0) (push a lst)))
           ((= a (* 2 a0)) (let ((opt (reverse lst)))
                                   (values (rest opt) (first opt)))))))

(defun is-perfect-square? (n)
    (let ((s (isqrt n)))
           (= (* s s) n)))

(defmacro convergents (var n &body body)
"Executes body for each convergent of the continued fraction of the 
square root of n. var is a variable that references the convergents.
NB. The convergent corresponding to the integer square root of 'n' is NOT applied."
   (let ((ring (gensym)) (a (gensym)) (num (gensym)) (den (gensym)))
  `(block Nil
   (let ((,ring (multiple-value-bind (repeat start) (continued-fraction ,n)
                     (cons start (setf (cdr (last repeat))
                                        repeat)))))
      (do* ((,a (cdr ,ring) (cdr ,a))
            (,num (list (1+ (* (car ,ring) (cadr ,ring))) (car ,ring))
                  (cons (+ (* (car ,num) (car ,a)) (cadr ,num)) (butlast ,num)))
            (,den (list (cadr ,ring) 1)
                  (cons (+ (* (car ,den) (car ,a)) (cadr ,den)) (butlast ,den)))
            (,var (/ (car ,num) (car ,den))
                  (/ (car ,num) (car ,den))))
            ((progn ,@body) ,var))))))

(defun minimal-pell-solution (d)
"Finds the minimum solution (x and y) for Pell's equation given d.
 Returns the values of x and y in that order"
   (convergents v d
         (when (= (- (expt (numerator v) 2)
                     (* d (expt (denominator v) 2))) 1)
                (return (values (numerator v) (denominator v))))))

(let ((nosquares (loop for i from 2 to 1000
                             unless (is-perfect-square? i)
                                collect i)))
  (values-list
   (reduce #'(lambda (x y) (if (> (car x) (car y)) x y))
           (mapcar #'(lambda (x) (list (minimal-pell-solution x) x)) nosquares))))
