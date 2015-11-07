;;;; Solution for problem 14 in Project Euler
;;;; https://projecteuler.net/problem=14
;;;; Paulo Mendes, 8-OCT-2015

(let ((longest-seq 0)
      (max-values 0)
      (collatz (make-hash-table :size 3000000)))

(defun n-collatz (n)
"Returns the number of steps in Collatz sequence n
 Memoized using hash table collatz "
   (or (gethash n collatz)        ; Entry 1 -> 1 serves as stopping criterion for recursion  
       (setf (gethash n collatz)
         (if (evenp n)
            (1+ (n-collatz (/ n 2)))
            (1+ (n-collatz (1+ (* n 3))))))))

  (setf (gethash 1 collatz) 1)
  (loop for i from 1 to 1000000
        for j = (n-collatz i)
        if (> j longest-seq) do (setf longest-seq j
                                      max-values i))
  (values longest-seq max-values))
