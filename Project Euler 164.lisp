;;;; Solution for Probem 164 in Project Euler
;;;; Paulo Mendes, 19-MAY-2016

(let ((memoize (make-array '(21 10 10) :initial-element Nil)))
   (defun count-numbers (n &optional (previous-numbers '(0 0)))
      (cond 
        ((= n 1)
          (- 9 (apply #'+ previous-numbers)))
        (t
          (or (aref memoize n (car previous-numbers) (cadr previous-numbers))
              (setf (aref memoize n (car previous-numbers) (cadr previous-numbers)) 
                   (loop for i from 0 to (- 9 (apply #'+ previous-numbers))
                    summing (count-numbers (1- n) (append (cdr previous-numbers)
                                                            (list i))))))))))
