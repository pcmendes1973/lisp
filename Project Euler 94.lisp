;;;; Solution for Problem 94 in Project Euler
;;;; Paulo Mendes, 23-MAR-2017

(do ((sum 0)
     (x 2 (+ (* 2 x) (* 3 y)))                 ; Pell's equation for D = 3
     (y 1 (+ (* 2 y) (* 1 x))))                ; Fundamental solution is (2,1).
    ('())                                      ; Loop until perimeter > 1000000000
      (let* ((small-triangle (/ (- x 2) 3))    
             (b (if (integerp small-triangle)  ; Hemibase of triangle for either
                       small-triangle          ; (x, x, x+1) or (x, x, x-1)
                     (+ small-triangle 4/3)))
             (perimeter (* 2 (+ b (isqrt (+ (expt b 2) (expt y 2)))))))
          (if (> perimeter 1000000000)
               (return sum)
             (if (> perimeter 2) (incf sum perimeter)))))
