;;;; Some utilities functions created to solve Project Euler problems and math in general
;;;;

(defun eratosthenes (n)
"Finds all primes smaller than n using the sieve of Eratosthenes. Returns an array
 containing the results."
   (let ((sieve (make-array (1- n) :initial-contents (loop for i from 2 to n collect i))))
      (loop for i across sieve
            if i do (loop for j from (expt i 2) to n by i
                          do (setf (aref sieve (- j 2)) Nil)))
    (values (remove-if #'null sieve))))

(defmacro do-permutations ((var vec &key append-into (len var ext-len)) &body body)
"Iterates over all permutations of a sequence. Permutations are calculated using
 Heap's algorithm.
          var: Variable that points to permutations.
          vec: Sequence that is permutated.
  append-into: Optional list into which results yielded by each call to 'body' are appended.
               The results of append-into are available to the code in 'body.'
         body: Code that is compiled and called for each permutation of 'vec'. 'Return' can be used.
   RETURN VALUES
      Returns the final value of append-into if it's used as a key, otherwise returns Nil."
  (let ((n (gensym)))
  `(block Nil
    (let ((,var ,vec) ,@(if append-into `((,append-into Nil))))
     (labels ((generate (&optional (,n ,(if ext-len len `(length ,var))))
                (if (= ,n 1)
                    ,(if append-into
                         `(setf ,append-into
                                 (append ,append-into (list (progn ,@body))))
                       `(progn ,@body))
                  (let ((n-1 (1- ,n)))
                    (loop for i from 0 below n-1
                          do (progn
                               (generate n-1)
                               (rotatef (elt ,var (if (evenp ,n) i 0))
                                        (elt ,var n-1))))
                    (generate n-1)))))
       (generate))
      ,@(if append-into `((values ,append-into)))))))
      
(defmacro do-primes (var n &rest body)
"Iterates over all primes smaller than n. Primes are calculated using the Sieve of
 Eratosthenes over a bit array.
   var: Variable that points to permutations.
     n: Limit to prime number list.
  body: Code that is compiled and called for each permutation of 'vec'. 'Return' can be used.
    RETURN VALUES
      Nil"
   (let ((i (gensym)))
  `(block Nil
    (let* ((array-size (1- ,n))
           (candidates (make-array array-size :element-type 'bit :initial-element 0)))
      (loop for ,i across candidates
            for ,var from 2
            if (zerop ,i) do (progn 
                         (loop for k from (- (expt ,var 2) 2) below array-size by ,var
                               do (setf (sbit candidates k) 1))
                         (progn ,@body)))))))
      
