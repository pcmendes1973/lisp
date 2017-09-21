;;;; Solution for Problem 58 in Project Euler
;;;; Paulo Mendes, 20-SEP-2017

(defun is-prime? (n) (loop for i from 3 to (1+ (isqrt n)) by 2 never (zerop (mod n i))))

(loop for i from 1
      for j = 9 then (+ j (* 8 i))
      summing (count t (mapcar #'is-prime?
                               (list (- j (* 6 i)) (- j (* 4 i)) (- j (* 2 i)) j))) into s
      when (< (/ s (+ 4 (* i 4))) 1/10) return (1+ (* 2 i)))
