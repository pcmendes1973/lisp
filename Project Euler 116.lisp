;;;; Solution for Problem 116 in Project Euler
;;;; Paulo Mendes, 31-MAY-2017
(defun count-color (length tile-length)
"Counts the number of ways in which a segment having a given length
 can be filled with one or more colored tiles having length 'tile-length'"
  (let ((memo (make-array (list length) :initial-element Nil)))
  (labels ((count-tiles (&optional (filled 0))
     (or (aref memo filled)
         (setf (aref memo filled)
         (loop for i in (list 1 tile-length)
               summing (cond
                        ((> (+ filled i) length) 0)
                        ((= (+ filled i) length) 1)
                        (t
                         (count-tiles (+ filled i)))))))))
     (1- (count-tiles)))))

(loop for i from 2 to 4 summing (count-color 50 i))
