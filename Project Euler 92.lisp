

;;;; Solution for problem 92 in Project Euler
;;;; https://projecteuler.net/problem=92
;;;; Paulo Mendes, 8-NOV-2015


(defvar *squares-of-digits* #(0 1 4 9 16 25 36 49 64 81))

(defun add-squares (n)
"Adds the squares of the digits in n"
  (if (< n 10)
    (aref *squares-of-digits* n)
   (multiple-value-bind (div mod)
     (floor n 10)
       (+ (add-squares div) (aref *squares-of-digits* mod)))))

(defvar question-hash-table (make-hash-table :size 10000000)
"Hash table for memoization of function arrives-at-89?")

(defun arrives-at-89? (n)
"Returns T if n arrives at 89, Nil if it arrives at 1"
   (or (gethash n question-hash-table)
   (setf (gethash n question-hash-table)
   (case n
     (1 Nil)
     (89 T)
     (otherwise (arrives-at-89? (add-squares n)))))))

(loop for i from 2 to 10000000
      counting (arrives-at-89? i))
