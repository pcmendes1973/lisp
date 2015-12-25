;;;; Solution for problem 113 in Project Euler
;;;; Paulo Mendes, 23-DEC-2015

(defun count-descendents (first-digit n-digits &optional lookup-table)
  (let ((lut (or lookup-table
                 (make-array (list (1+ n-digits) (1+ first-digit))
                             :initial-element Nil))))
  (cond
    ((= 1 n-digits) 1)
    ((if lookup-table
       (aref lookup-table n-digits first-digit)))
    (t (setf (aref lut n-digits first-digit)
                (loop for i from first-digit downto 0
                      summing (count-descendents i (1- n-digits) lut)))))))

(defun count-non-bouncy (n-digits)
   (let ((upper-bound (1+ n-digits))
         (lut (make-array (list (+ n-digits 2) 10)
                             :initial-element Nil)))
   (+ (count-descendents 9 upper-bound lut)       ; Add ascending numbers 
      (loop for i from (1+ n-digits) downto 1
            summing (count-descendents 9 i lut))  ; Add descending numbers
      (- n-digits)                                ; Subtract 00, 000, &c. overcounted as descending
      (1- (* 9 n-digits)))))                      ; Subtract double-counted numbers
