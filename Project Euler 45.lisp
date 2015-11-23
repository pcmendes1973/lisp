;;;; Solution for problem 45 in Project Euler
;;;; Paulo Mendes, 23-NOV-2015

(loop for np = 166 then (if (< pn hn) (1+ np) np)
      for pn = (/ (* np (1- (* 3 np))) 2)
      for nh = 143 then (if (< hn pn) (1+ nh) nh)
      for hn = (* nh (1- (* 2 nh)))
      when (= pn hn) return pn)
