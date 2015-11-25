;;;; Solution for Problem 62 in Project Euler
;;;; Paulo Mendes, 25-NOV-2015

(defun digits (n &key (base 10)
                      (append Nil))
"Returns the digits of n in a list
 Keyword 'append' is used solely for recursion"
  (if (< n base)
    (cons n append)
   (multiple-value-bind (div mod)
     (floor n base)
     (digits div :base base
                 :append (cons mod append)))))

(defun is-permutation? (x y)
"Checks whether the digits of x and y are permutations of each other"
  (equal (sort (digits x) #'>)
         (sort (digits y) #'>)))

(defun list-of-cubes (n-digits)
"Returns a list of cubes with n-digits"
  (loop for i from (ceiling (expt 10 (/ (1- n-digits) 3))) to
                   (floor (expt 10 (/ n-digits 3)))
        collect (expt i 3)))

(loop named main
      for number-of-digits from 7
      for j = (list-of-cubes number-of-digits)
      do (maplist #'(lambda (x) 
             (if (= 4 (count-if #'(lambda (y) (is-permutation? y (car x))) (cdr x)))
               (return-from main (car x)))) j))
