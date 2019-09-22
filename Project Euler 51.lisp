;;;; Solution for problem 51 in Project Euler
;;;; Paulo Mendes, 22-SEP-2019

(defun expt-mod (a b mod)
"Returns a ^ b mod 'mod'"
  (if (zerop b) 1
    (multiple-value-bind (div rem)
       (floor b 2)
         (let ((m (expt-mod a div mod)))
           (mod (* (mod (expt m 2) mod) (if (zerop rem) 1 a)) mod)))))

(defun primep (n &optional (base 2))
"Primality test using Fermat's little theorem. Returns T when base^n = base mod n, Nil otherwise"
  (= (expt-mod base n n) base))


(defmacro test-mask (count mask)
"Tests whether shifting the digits in 'mask' can produce an 8-member prime family."
  (with-gensyms (n zeros)
   `(loop with ,n = ,count
          with ,zeros = (list ,@mask)
          with symbols = (loop for i from 0 below ,n collect (gensym))
          with output-list = (loop for i from 0 below ,n
                                    for j in symbols unless (find i ,zeros) collect j)
          with increment = (reduce #'+ (mapcar #'(lambda (x) (expt 10 x)) ,zeros))
          for i from 0 below ,n
          for code = `(loop for ,(car symbols) in '(1 3 7 9)
                            when (loop repeat 10 for j from (+ ,@output-list) by ,increment
                                       for pr = (primep j)
                                       counting pr into n when pr minimizing j into min
                                       when (and (= n 8) (> min ,(expt 10 (1- ,n)))) do (return min)) collect it)
                   then (if (find i ,zeros) code `(loop repeat 10 for ,(nth i symbols) from 0 by ,(expt 10 i) nconc ,code))
          finally (return (eval code)))))
