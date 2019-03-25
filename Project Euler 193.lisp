;;;; Solution for problem 193 in Project Euler
;;;; Paulo Mendes, 7-Mar-2019

(defmacro do-odd-primes ((var max &key (min 3) (action 'do) (aux-var Nil aux-var-provided?)) &body body)
  (with-gensyms (imin imax n m i j pos buffer)
    `(let* ((,imax ,max) (,imin (max 3 ,min)) (,n (floor (1- ,imax) 2)) (,m (floor (1- ,imin) 2)) ,@(if aux-var-provided? `(,aux-var)))
       (let ((,buffer (make-array (list (- ,n ,m -1)) :element-type 'bit :initial-element 0)))
         (loop for ,i from 1 to (floor (1- (isqrt ,imax)) 2)
            do (loop for ,j from (max ,i (ceiling (- ,m ,i) (1+ (* 2 ,i))))
                  for ,pos = (- (+ ,i ,j (* 2 ,i ,j)) ,m)
                  until (> ,pos (- ,n ,m)) do (setf (aref ,buffer ,pos) 1))
            finally (return (loop for ,i across ,buffer for ,j from ,m
                                  when (zerop ,i) ,action (let ((,var (1+ (* 2 ,j)))) (progn ,@body)))))))))

(let ((l (list (expt 2 50) (- (expt 2 48)))) (sum 0))
  (do-odd-primes (p (isqrt (car l)))
    (let ((p2 (expt p 2)))
      (setf l (mapcan #'(lambda (x) (let ((absx (abs x)))
                                          (if (> p2 absx) (prog1 Nil (incf sum x))
                                              (list x (- (* (signum x) (floor absx p2)))))))
                                      l))))
        (values (reduce #'+ l :initial-value sum)))
