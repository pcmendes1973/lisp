;;;; Solution for Problem 151 in Project Euler
;;;; Paulo Mendes, 28-MAY-2016

(let ((memo (make-hash-table)))
(defun count-sheets (initial-contents &optional
                                         (prob 1.0d0) ; 1 is slower, 1.0 is wrong!
                                         (no-of-sizes (length initial-contents)))
  (let ((n-sheets (reduce #'+ initial-contents))
        (memo-key (+ prob (reduce #'(lambda (x y) (+ (* 10 x) y) ) initial-contents))))
  (if (zerop n-sheets) 0
  (or (gethash memo-key memo)
    (setf (gethash memo-key memo)  (+ (if (= n-sheets 1) prob 0)
  (loop for i in (maplist #'list initial-contents)
        for j from 1
        when (> (caar i) 0)
          summing (count-sheets
                      (mapcar #'+
                               initial-contents
                               (loop for k from 1 to no-of-sizes
                                     collect (cond ((= k j) -1)
                                                   ((< k j)  0)
                                                    (t 1))))
                  (* prob (/ (caar i) n-sheets)))))))))))

(format t "~0,6F~%" (- (count-sheets '(1 0 0 0 0)) 2))
