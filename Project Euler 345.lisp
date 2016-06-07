;;;; Solution for problem 345 in Project Euler
;;;; Paulo Mendes, 06-JUN-2016

(defun matrix-sum (mat &aux (nrows (array-dimension mat 0)))
   (let ((maximum-sum 0)
         (biggest-increase (loop for i from 14 downto 0
                                 summing (loop for j from 0 to 14
                                               maximizing (aref *matrix* j i)) into r
                                               collect r into l
                            finally (return (coerce (reverse l) 'array)))))
   (labels ((matrix-iterate (&optional
                                 (sum 0)
                                 (previous-rows Nil)
                                 (col 0))
   (cond 
     ((= (length previous-rows) nrows)
            (setf maximum-sum (max maximum-sum sum)))
     ((< (+ sum (aref biggest-increase col))
                maximum-sum) 0)
     (t
       (loop for row from 0 below nrows
             unless (find row previous-rows)
             maximize (matrix-iterate (+ sum (aref mat row col))
                                      (cons row previous-rows)
                                      (1+ col)))))))
   (matrix-iterate))
   (values maximum-sum)))
