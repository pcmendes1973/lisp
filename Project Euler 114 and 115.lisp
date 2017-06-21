;;;; Solution for Problems 114 and 115 in Project Euler
;;;; Paulo Mendes, 21-JUN-2017

(defun f (p q)
  (let ((memo (make-array (list (1+ q) 2) :initial-element Nil)))
     (labels ((count-ways (n &optional previous-red?)
                 (or (aref memo n (if previous-red? 0 1))
                     (setf (aref memo n (if previous-red? 0 1))
                     (cond
                       ((= n q) 1)
                         (previous-red? (count-ways (1+ n)))
                       (t
                         (loop for red-block-length from p to (- q n)
                               summing (count-ways (+ n red-block-length) t) into s
                               finally (return (+ s (count-ways (1+ n)))))))))))

    (count-ways 0))))
