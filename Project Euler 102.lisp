

;;;; Solution for Project Euler Problem 102
;;;; Paulo Mendes, 25-JUN-2016

(defun point-in-triangle? (a b c p)
"T if point p is inside triangle abc, Nil otherwise.
 All points are represented as 2-unit vectors"
  (flet ((dot-product (a b) (reduce #'+ (map 'vector #'* a b)))
         (subtract (a b) (map 'vector #'- a b)))
   (let* ((v0 (subtract c a))
          (v1 (subtract b a))
          (v2 (subtract p a))
          (dot00 (dot-product v0 v0))
          (dot01 (dot-product v0 v1))
          (dot02 (dot-product v0 v2))
          (dot11 (dot-product v1 v1))
          (dot12 (dot-product  v1 v2))
          (denom (- (* dot00 dot11) (* dot01 dot01)))
          (u (/ (- (* dot11 dot02) (* dot01 dot12)) denom))
          (v (/ (- (* dot00 dot12) (* dot01 dot02)) denom)))
   (and (>= u 0) (>= v 0) (<= (+ u v) 1)))))


(let ((in (open "C:\\Users\\Paulo Mendes\\Desktop\\Programming\\Lisp\\p102_triangles.txt"
                :if-does-not-exist nil)))
   (when in
     (loop for line = (read-line in nil)
           while line
             counting (let* ((numbers (map 'vector #'parse-integer (split-sequence "," line)))
                             (a (make-array 2 :displaced-to numbers
                                              :displaced-index-offset 0))
                             (b (make-array 2 :displaced-to numbers
                                              :displaced-index-offset 2))
                             (c (make-array 2 :displaced-to numbers
                                              :displaced-index-offset 4)))
                         (point-in-triangle? a b c (vector 0 0))) into triangles
           finally (return triangles)
     (close in))))
