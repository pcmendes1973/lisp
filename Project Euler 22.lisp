;;;; Solution for problem 22 in Project Euler

(setf names... ; Contains names pasted from file in list of arrays.

(loop for i in (sort names #'string-lessp)
      for j from 1
      summing (* j (reduce #'+ (map 'vector #'(lambda (x) (- (char-code x) 64)) i))))
