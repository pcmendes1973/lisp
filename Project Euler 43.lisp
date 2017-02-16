;;;; Solution to Problem 43 in Project Euler
;;;; Paulo Mendes, 15-FEB-2017

;;; Anaphoric version
;;; To make this macro non-anaphoric, change (,var ,vec) to (,var (copy-seq ,vec))
(defmacro run-permutations (var vec &rest body)
"Uses Heap's algorithms to generate all permutations of sequence vec,
 puts the results in variable var and runs code in body for each
 permutation"
  `(let ((,var ,vec))
     (labels ((generate (&optional (n (length ,var)))
       (if (= n 1)
         (progn ,@body)
        (progn 
           (loop for i from 0 below (1- n)
                 do (progn 
                      (generate (1- n))
                      (rotatef (aref ,var (if (evenp n) i 0))
                               (aref ,var (1- n)))))
           (generate (1- n))))))
           
 ;;; Uses the run-permutations macro to generate all possible permutations
 ;;; of 10 digits and tests them against a sequence of closures within an
 ;;; and macro.
 (defmacro euler43 ()
   `(let* ((sum-total 0))
        (run-permutations num #(0 1 2 3 4 5 6 7 8 9)
           (and ,@(loop for i in '(17 13 11 7 5 3 2)
                        for j from 7 downto 1
                        collect `(zerop (mod (+ (* 100 (aref num ,j))
                                                (* 10  (aref num ,(+ j 1)))
                                                       (aref num ,(+ j 2)))
                                         ,i)))
                (incf sum-total
                      (reduce #'+
                              (map 'vector
                                   #'*
                                   num
                                   #(,@(loop repeat 10
                                             for i = 100000000 then (/ i 10)
                                             collect i)))))))
     (values sum-total)))
 
