;;;; Solution for Problem 91 in Project Euler
;;;; Paulo Mendes, 7-JUN-2017

(let ((points (loop for i from 0 to 50
                    nconc (loop for j from 0 to 50
                                collect (cons i j)))))
   (defun square-distance (point1 point2)
       (+ (expt (- (car point1) (car point2)) 2)
          (expt (- (cdr point1) (cdr point2)) 2)))
   (defun is-right-triangle? (point1 point2)
       (let ((distances (sort (list (square-distance '(0 . 0) point1)
                                    (square-distance point1 point2)
                                    (square-distance '(0 . 0) point2)) #'>)))
            (and (notany #'zerop distances)
                 (= (car distances) (+ (cadr distances) (caddr distances))))))
   (loop for i in (butlast (mapcon #'list points))
         summing (loop for j in (cdr i)
                       counting (is-right-triangle? (car i) j))))
