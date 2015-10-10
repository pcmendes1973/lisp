;;;; Project Euler Problem 1
;;;; https://projecteuler.net/problem=1
(loop for i from 1 to 1000
    summing (if (or (= (mod i 3) 0) (= (mod i 5) 0)) i 0))

;;;; Project Euler Problem 2
;;;; https://projecteuler.net/problem=2
(loop for i = 0 then j
      and j = 1 then (+ i j)
      until (> j 4000000)
      summing (if (evenp j) j 0))
