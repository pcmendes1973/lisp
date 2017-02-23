;;;; Solution for Problem 86 in Project Euler
;;;; 23-FEB-2017

(require 'cl-ppcre)

(defun get-matrix (str)
  (with-open-file (stream str)
    (when stream
      (loop for line = (read-line stream nil)
            while line
               collect (mapcar #'parse-integer (cl-ppcre:split "," line)) into data
             counting line into lines
             finally (return (make-array (list (length (car data)) lines)
                                         :initial-contents data))))))

(let* ((matrix (get-matrix "p083_matrix.txt"))
       (memo (make-array (array-dimensions matrix) :initial-element 10000000000))
       (isize (car (array-dimensions matrix)))
       (jsize (cadr (array-dimensions matrix)))
       (distance-to-end 10000000000))
  (defun shortest-path (i j &optional (sum-previous 0))
    (let ((path-length (+ sum-previous (aref matrix i j))))
      (if (or (< (aref memo i j) path-length) (< distance-to-end path-length))
          (return-from shortest-path 10000000000)
        (setf (aref memo i j) path-length))
      (if (and (= (1- isize) i) (= (1- jsize) j))
          (setf distance-to-end (min (aref memo i j) path-length))
        (reduce #'min
                (mapcan #'(lambda (x y)
                            (and (> isize x -1) (> jsize y -1) (cons (shortest-path x y path-length) nil)))
                        (list i (1+ i) i (1- i))
                        (list (1+ j) j (1- j) j)))))))
