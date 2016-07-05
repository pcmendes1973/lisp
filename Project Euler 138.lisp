;;;; Solution for Problem 183 in Project Euler
;;;; Paulo Mendes, 5-JUL-2016

(defun is-perfect-square? (x)
   (eq x (expt (isqrt x) 2)))

(defun n (m)
   (let* ((p1 (+ (* 5 (expt m 2)) 1))
          (p2 (- (* 5 (expt m 2)) 1))
          (delta (or
              (and (is-perfect-square? p1) (isqrt p1))
              (and (is-perfect-square? p2) (isqrt p2)))))
     (if delta
        (let ((n (- delta (* 2 m))))
           (values (- (expt m 2) (expt n 2))
                   (* 4 m n)
                   (+ (expt m 2) (expt n 2)))))))

(loop for i from 2
      when (n i) collect (multiple-value-list (n i)) into triangles
      until (eq (length triangles) 12)
      finally (return
                 (reduce #'(lambda (x y) (+ x (caddr y))) triangles :initial-value 0)))
