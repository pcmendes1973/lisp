;;;; Solution for Problem 348 in Project Euler
;;;; Paulo Mendes, 11-AUG-2016

(defun is-palindrome? (n)
   (let ((digits (princ-to-string n)))
      (string= (reverse digits) digits)))

(loop with c = 1000000000
      with palindromes = (make-hash-table)
      for x from 1 below (sqrt (1- c))
      for row = Nil then
                    (merge 'list row
                       (loop for y from 1 below (expt (- c (expt x 2)) 1/3)
                             for z = (+ (expt x 2) (expt y 3))
                                when (is-palindrome? z)
                                   do (setf (gethash z palindromes)
                                            (if (gethash z palindromes)
                                                  (1+ (gethash z palindromes)) 1))) #'<)
      finally (return
                 (loop for v being the hash-values in palindromes 
                       using (hash-key k)
                         when (= v 4) collect k)))
