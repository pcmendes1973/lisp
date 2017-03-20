;;;; Solution for Problem 35 in Project Euler
;;;; Paulo Mendes, 19-MAR-2017

(defun eratosthenes (n)
"Calculates all primes smaller than n using the sieve of Eratosthenes"
   (let ((sieve (make-array (1- n) :initial-contents (loop for i from 2 to n collect i))))
      (loop for i across sieve
            if i do (loop for j from (expt i 2) to n by i
                          do (setf (aref sieve (- j 2)) Nil)))
    (values (remove-if #'null sieve))))

(defparameter primes (eratosthenes 1000000))

(defun rotations (n)
"Returns all rotations of 'n' sorted ascendingly."
  (do ((rot Nil)
       (k 0 (incf k))
       (i n (multiple-value-bind (q r) (floor i 10)
                 (prog1 q (push r rot)))))
     ((zerop i) (loop repeat k
                      collect (setf rot
                                    (cons (car (last rot))
                                          (butlast rot))) into output
                      finally (return
                                  (sort (mapcar #'(lambda (x)
                                                (reduce #'(lambda (y z) (+ z (* 10 y))) x)) 
                                                 output) #'<))))))

(defun find-in-ordered-array (n arr &optional (start 0) (end (1- (length arr))))
"Searches array 'arr' and returns the position of 'n' if 'arr' contains 'n', Nil otherwise."
     (cond
        ((= start end) 
           (if (= n (aref arr start)) start))
        ((= (- end start) 1)
           (if (= n (aref arr end)) end 
              (if (= n (aref arr start)) start)))
        (t
          (let ((midpoint (+ start (floor (- end start) 2))))
              (if (>= n (aref arr midpoint))
                   (find-in-ordered-array n arr midpoint end)
                 (find-in-ordered-array n arr start (1- midpoint)))))))

(loop for i across primes
      counting (every #'(lambda (x)
                           (find-in-ordered-array x primes))
                      (rotations i)))
