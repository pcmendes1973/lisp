;;;; Solution for Problem 64 in Project Euler
;;;; Paulo Mendes, 18-MAR-2017

(defun continued-fraction (n)
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

(loop for i from 2 to 10000
      unless (is-perfect-square? i)
      count (oddp (length (continued-fraction i))))
