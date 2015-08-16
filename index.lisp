;;;; Returns an index for list lst. Accepts comparison function as second argument.
;;;;
;;;; Paulo Mendes, 07/JUL/2015


(defun index (lst &optional (func #' <))
"Returns an index for list lst. Accepts comparison function as second argument."
  (loop for i in lst
        for j from 0 collect `(,i ,j) into indexed
   finally (return (loop for i in (sort indexed
                                      #'(lambda(x y) (funcall func (car x) (car y)))) collect (cadr i)))))
