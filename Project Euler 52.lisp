;;;; Solution for problem 52 in Project Euler
;;;; Paulo Mendes, 3-DEC-2015


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

(defun is-permutation? (&rest numbers)
"Returns T if all numbers are permutations of each other, Nil otherwise"
   (let* ((lsts (mapcar #'digits numbers))
          (sorted-lists (mapcar #'(lambda (x) (sort x #'>)) lsts)))
     (loop for i in (cdr sorted-lists)
           always (equalp (car sorted-lists) i))))

(loop named outer
      for digits = 100000 then (* 10 digits) do  
(loop for i from digits to (floor (* 10 digits) 6)
      if (apply #'is-permutation?
                (mapcar #'(lambda (x) (* x i)) '(1 2 3 4 5 6)))
          do (return-from outer i)))
