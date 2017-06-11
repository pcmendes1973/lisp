;;;; Solution for Problem 173 in Project Euler
;;;; Paulo Mendes, 7-JUN-2017

(loop for a1 from 8 by 4
      while (<= a1 1000000)
        summing (loop for q from a1 by 8
                      summing q into s
                      when (> s 1000000)
                         return c
                      counting t into c))

;;; Slower solution using sqrt instead of just adding the laminae
(loop for a1 from 8 by 4
      while (<= a1 1000000)
        summing (floor (* 1/8 (- (sqrt (+ (* a1 a1) (* -8 a1) 16000016)) a1 -4))))
