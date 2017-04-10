;;;; Solution for Problem 597 in Project Euler
;;;; Paulo Mendes, 10-APR-2017

(defun area-under-circle (x)
   (- 1
   (/ (- (* (1- x) (sqrt (- (* 2 x) (expt x 2))))
         (* 2 x)
         (asin (/ (- 2 (* 2 x)) 2))) -2)))

(defun concave-triangle-area (n)
   (let* ((x (/ (- (+ (expt n 2) n)
                   (* (sqrt 2) (expt n 3/2)))
                (1+ (expt n 2)))))
      (+ (area-under-circle x) (* x x (/ n) 1/2))))
