;;;; Solutiuon for Problem 30 in Project Euler
;;;; Paulo Mendes, 20-NOV-2015

(let ((barning-matrixes
'(((1 -2 2)
   (2 -1 2)
   (2 -2 3))
  ((1 2 2)
   (2 1 2)
   (2 2 3))
  ((-1 2 2)
   (-2 1 2)
   (-2 2 3))))
     (triplets-count (make-array 1000)))

(defun matrix-multiply (mat vec)
  (mapcar #'(lambda (x) (apply #'+ (mapcar #'* x vec))) mat))

(defun parse-node (triplet &key (max-perimeter 1000)) 
  (let ((perimeter (apply #'+ triplet)))
     (if (> perimeter max-perimeter) (return-from parse-node Nil))       ; Terminating condition
     (loop for i from perimeter by perimeter                             ; Increments counts for triplet
           while (< i max-perimeter) do (incf (svref triplets-count i))) ; and its multiples until perimeter < 1000
     (loop for i in (mapcar #'(lambda (x) (matrix-multiply x triplet)) barning-matrixes)
           do (parse-node i))))

(parse-node '(3 4 5))

(position (reduce #'max triplets-count) triplets-count))
