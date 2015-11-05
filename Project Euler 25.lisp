;;;; Solution for problem 25 in Project Euler
;;;; https://projecteuler.net/problem=25
;;;; Paulo Mendes, 10-OCT-2015


(loop for i = 0 then j
      and j = 1 then (+ i j) 
      and k from 1 maximize k
      until (> j (expt 10 1000)))
