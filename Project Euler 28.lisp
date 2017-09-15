;;;; Solution for Problem 28 in Project Euler
;;;; Paulo Mendes, 15-SEP-2017

(loop for i from 0 to 4000 by 8      ; 4000 is ((diagonals - 1) / 2) * 8
      for j = 1 then (+ j i)         ; to iterate once for each outer square.
      summing (- (* j 4) (* i 3/2)) into s
      finally (return (- s 3)))      ; Compensate for central '1' being counted 4 times
