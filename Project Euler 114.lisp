;;;; Solutiom for problem 114 in Project Euler
;;;; Paulo Mendes, 10-JUN-2017

(defparameter block-length 50)

(let ((memo (make-array '(51 2) :initial-element Nil)))
(defun count-ways (n &optional previous-red?)
   (or (aref memo n (if previous-red? 0 1))
       (setf (aref memo n (if previous-red? 0 1))
         (cond
           ((= n block-length) 1)
           (previous-red? (count-ways (1+ n)))
           (t
             (loop for red-block-length from 3 to (- block-length n)
                   summing (count-ways (+ n red-block-length) t) into s
                   finally (return (+ s (count-ways (1+ n)))))))))))
