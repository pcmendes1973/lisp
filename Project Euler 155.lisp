;;;; Solution for Problem 155 in Project Euler
;;;; Paulo Mendes 4-JUN-2016

(loop with memo = (make-array 1000000 :initial-element 0)
      for z from 1 to 750000
      do (loop for delta from (ceiling z 3)
                  for n = (* (- (- delta) z) (- z (* 3 delta)))
                  while (< n 1000000) do (incf (aref memo n)))
      finally (return (count 10 memo)))
