;;;; Solution for problem 207 in Project Euler
;;;; Paulo Mendes 20-NOV-2016

(defun is-2n? (n) 
   (if (= n 1) t
      (and (evenp n) (is-2n? (/ n 2)))))

(loop for i from 2
      counting (is-2n? i) into perfect-partitions
      when (< (/ perfect-partitions (1- i)) 1/12345)
         return (- (expt i 2) i))
