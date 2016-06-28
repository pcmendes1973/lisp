;;;; Solution for Problem 197 in Project Euler
;;;; Paulo Mendes 28-JUN-2016

(loop for i from 1 to 1000
      for j = 0.71d0 then (* (floor (expt 2 (- 30.403243784d0 (expt j 2)))) (expt 10d0 -9))
      finally (return (+ j (f j))))
