;;;; Solution for Problem 2 in Project Euler
;;;; Paulo Mendes, 17/05/2017

(defmacro do-primes (var n &rest body)
   (let ((i (gensym)))
  `(block Nil
    (let* ((array-size (1- ,n))
          (candidates (make-array array-size :element-type 'bit :initial-element 0)))
      (loop for ,i across candidates
            for ,var from 2
            if (zerop ,i) do (progn 
                         (loop for k from (- (expt ,var 2) 2) below array-size by ,var
                               do (setf (sbit candidates k) 1))
                         (progn ,@body)))))))


(let ((maxp 0) (biggest-repetend 0))
  (do-primes p 1000
    (let ((p-repetend (loop for j = 10 then (* j 10) ; Calculates the repetend for p
                            for k = (/ (1- j) p)
                            when (or (integerp (/ j p)) (integerp k))
                               return k)))
       (if (> p-repetend biggest-repetend)           ; Finds the biggest repetend
          (setf maxp p                               ; and the corresponding p (maxp-p)
                biggest-repetend p-repetend))))
    (values maxp biggest-repetend))
