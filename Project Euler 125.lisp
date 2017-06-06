;;;; Solution for problem 125 in Project Euler
;;;; Paulo Mendes, 06-JUN-2017

(defun is-palindrome? (n)
   (let ((digits (princ-to-string n)))
      (loop for i from 0
            for j from (1- (length digits)) by -1
            while (> j i)
            always (eq (aref digits j) (aref digits i)))))

(defun sum-palindromes (n)
   (loop for start from 1 to (* 2 (isqrt n))
         nconc (loop for i from start and q = (expt i 2)
                     summing q into s
                     until (> s n)
                     when (and (> s q) (is-palindrome? s))
                        collect s) into nums
         finally (return (reduce #'+ (remove-duplicates nums)))))
