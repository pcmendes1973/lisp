;;;; Non-destructive quicksort in a cool 5 lines
;;;;
;;;; Paulo Mendes, 04/JUL/2015

(defun qsort (lst)
"Non-destructive quicksort"
  (if (null (cdr lst)) (return-from qsort (values lst)))
    (loop for i in lst if (> i (car lst)) collect i into greater
                       else collect i into lequal
     finally (return (if greater (return `(,@(qsort lequal) ,@(qsort greater)))
                                 (return `(,@(qsort (cdr lequal)) ,(first lequal)))))))
