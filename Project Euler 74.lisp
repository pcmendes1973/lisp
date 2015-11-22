;;;; Solution for problem 74 in Project Euler
;;;; Paulo Mendes, 22-NOV-2015


(let ((factorials #(1 1 2 6 24 120 720 5040 40320 362880))
      (memo (make-hash-table :rehash-size 1000000)))

(defun sum-of-factorials (n)
   (or (gethash n memo)
       (setf (gethash n memo) (if (< n 10)
                                     (svref factorials n)
                                    (multiple-value-bind (div mod)
                                      (floor n 10)
                                      (+ (svref factorials mod)
                                      (sum-of-factorials div))))))))

(defun number-of-terms (n)
  (loop for i from 0
        for j = n then (sum-of-factorials j)
        until (member j non-repeating)
        collect j into non-repeating
        finally (return i)))


(loop for i from 1 to 1000000 count (= (number-of-terms i) 60))
[/code]
