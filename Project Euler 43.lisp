;;;; Solution to Problem 43 in Project Euler
;;;; Paulo Mendes, 15-FEB-2017 

(defmacro do-permutations ((var vec &key anaphoric (len (length vec))) &body body)
 "Generates all permutations of vec, puts results into var and executes body for 
  each permutation.
   KEYS:
     anaphoric: if defined, modifies var outside the macro, preserves it otherwise
     len: number of items that will be permuted, default is the full vector"
  (let ((n (gensym)))
  `(let ((,var ,(if anaphoric vec `(copy-seq ,vec))))
     (labels ((generate (&optional (,n ,len))
                (if (= ,n 1)
                    (progn ,@body)
                  (let ((n-1 (1- ,n)))
                    (loop for i from 0 below n-1
                          do (progn 
                               (generate n-1)
                               (rotatef (aref ,var (if (evenp ,n) i 0))
                                        (aref ,var n-1))))
                    (generate n-1)))))
       (generate))))))

         
 ;;; Uses the run-permutations macro to generate all possible permutations
 ;;; of 10 digits and tests them against a sequence of closures within an
 ;;; and macro.
 (defmacro euler43 ()
   `(let* ((sum-total 0))
        (do-permutations num #(0 1 2 3 4 5 6 7 8 9)
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
 
