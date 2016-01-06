;;;; Solution for Problem 78 in Project Euler
;;;; Paulo Mendes, 5-JAN-2016

(let ((*partition* (make-array 100000)))

(setf (aref *partition* 0) 1)

(defun pentagonal (n)
  (/ (* (1- (* 3 n)) n) 2))

(defun partition (v)
(loop for n from 1
      for p1 = (- v (pentagonal n))
      for p2 = (- v (pentagonal (- n)))
      for sign = 1 then (* sign -1)
      until (< p1 0)
      summing (* sign (aref *partition* p1))
      until (< p2 0)
      summing (* sign (aref *partition* p2))))

(loop for n from 1
      for p = (mod (partition n) 1000000)
      do (setf (aref *partition* n) p)
      if (zerop p) return n))
