;;;; Solution for Problem 32 in Project Euler
;;;; Paulo Mendes, 22-MAR-2017

(defun result (n)
   (reduce #'(lambda (x y) (+ (* 10 x) y)) n))

(let* ((digits #(1 2 3 4 5 6 7 8 9))
       (md1 (make-array 2 :displaced-to digits :displaced-index-offset 0))
       (mr1 (make-array 3 :displaced-to digits :displaced-index-offset 2))
       (p1  (make-array 4 :displaced-to digits :displaced-index-offset 5))
       (md2 (make-array 1 :displaced-to digits :displaced-index-offset 0))
       (mr2 (make-array 4 :displaced-to digits :displaced-index-offset 1))
       (results))
    (do-permutations (v digits)
         (let ((r (result p1)) (m1 (result md1)) (m2 (result mr1))
                               (n1 (result md2)) (n2 (result mr2)))
           (when (or (= r (* m1 m2)) (= r (* n1 n2)))
                (pushnew r results))))
    (reduce #'+ results))
