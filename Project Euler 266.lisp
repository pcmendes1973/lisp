;;;; Solution for Problem 266 in Project Euler
;;;; Paulo Mendes, 11-Ago-2019

(let* ((primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83
                 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181))
       (upper-limit (isqrt (reduce #'* primes))))
  (defun list-all-products (input comp &key (condition #'identity))
    (loop with lst = '(1)
          for i in input do (loop for j in lst
                                  for prod = (* i j)
                                  when (funcall condition prod)
                                    collect prod into list1
                                  finally (setf lst (merge 'list list1 lst comp)))
       finally (return lst)))
  (loop with upper-half = (list-all-products (subseq primes 21) #'> :condition #'(lambda (x) (< x upper-limit)))
        and lower-half = (list-all-products (subseq primes 0 21) #'<)
        for i in lower-half maximize (loop for j on upper-half
                                           for prod bignum = (* (car j) i)
                                           when (> upper-limit prod)
                                              return (prog1 prod (setf upper-half j))
                                           finally (error "End of loop reached!")))) ; Necessary to pass type checking in SBCL Lisp

