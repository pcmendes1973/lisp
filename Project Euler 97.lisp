;;;; Solution for problem 97 in Project Euler
;;;; Paulo Mendes, 12-DEC-2015

(defun exponent-modulo (n exponent modulo)
   (cond
      ((= exponent 1) (mod n modulo))
      (t (multiple-value-bind (div mod)
            (floor exponent 2)
            (mod (* (expt (exponent-modulo n div modulo) 2)
                    (if (= mod 1) n 1)) modulo)))))
