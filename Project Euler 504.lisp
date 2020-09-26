;;;; Solution for project 504 in Project Euler
;;;; Paulo Mendes, 26-SEP-2020

(defparameter n 100)

(let ((h (make-hash-table)))
(defun count-points (a b)
    (declare (fixnum a b))
    (if (< a b)
        (count-points b a)
        (let ((index (+ (* a 1000) b)))
            (or (gethash index h)
                (setf (gethash index h) 
                      (loop for y fixnum from 1 below b
                            summing (let ((s (* (- b y) (/ a b))))
                                         (if (= (denominator s) 1)
                                             (1- s)
                                             (floor s))))))))))

(defun count-polygon-points (a b c d)
    (declare (fixnum a b c d))
    (+ a b c d -3
       (count-points a b)
       (count-points b c)
       (count-points c d)
       (count-points d a)))

(defun perfect-squarep (n)
    (declare (fixnum n))
    (and (> n 0)
         (let ((s (isqrt n)))
              (= n (* s s)))))

(loop for a fixnum from 1 to n
   summing (loop for b fixnum from 1 to n
              summing (loop for c fixnum from 1 to n
                         summing (loop for d fixnum from 1 to n
                                    counting (perfect-squarep (count-polygon-points a b c d))))))
