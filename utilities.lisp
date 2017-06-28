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
          var: Variable to which permutations are bound.
          vec: Sequence that is permutated.
  append-into: Optional list into which results yielded by each call to 'body' are appended.
               The results of append-into are available to the code in 'body.'
         body: Form that is evaluated for each permutation of 'vec'. 'Return' can be
               used to interrupt the iterations and return a value.
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
      
(defmacro do-primes ((var n &key collect) &rest body)
"Iterates over all primes smaller than n. Primes are calculated using the Sieve of
 Eratosthenes over a bit array.
    var: Variable to which permutations are bound.
      n: Limit to prime number list.
collect: If true, collects all results of evaluations of 'body' in the order that
         they are performed.
   body: Form that is evaluated for each permutation of 'vec'. 'Return' can be used
         to interrupt the iterations and return a value.
    RETURN VALUES
      Nil"
   (let ((i (gensym)))
  `(block Nil
    (let* ((array-size (1- ,n))
           (candidates (make-array array-size :element-type 'bit :initial-element 0)))
      (loop for ,i across candidates
            for ,var from 2
            when (zerop ,i) ,(if collect 'collect 'do) (progn 
                         (loop for k from (- (expt ,var 2) 2) below array-size by ,var
                               do (setf (sbit candidates k) 1))
                         (progn ,@body)))))))

(defmacro do-all-combinations ((var elements &key collect) &body body)
"Iterates over all combinations of sequence 'elements'. Combinations are
 tested in reverse binary order.
     var: Variable to which combinations are bound.
elements: Sequence from which combinations are drawn.
    body: Form that is evaluated for each combination. 'Return' can be
          used to interrupt the iterations and return a value.
 collect: If true, collects all results of evaluations of 'body' in the order that
          they are performed.
RETURN VALUES
 A list with the values collected as 'body' is evaluated if 'collect' is true, Nil otherwise."
  (let ((i (gensym)) (len (gensym)))
   `(block Nil
      (let* ((lst ,elements)
             (,len (length lst))
             (digits (make-array ,len :element-type 'bit :initial-element 0)))
         (labels ((iterate (&optional (pos 0))
                     (cond
                       ((> pos ,len)
                         (loop for i from 0 to ,len
                               do (setf (aref digits i) 0)))
                       ((zerop (aref digits pos))
                         (setf (aref digits pos) 1)
                         (loop for i from 0 below pos
                               do (setf (aref digits i) 0)))
                       (t
                         (iterate (1+ pos))))
                         (remove-if #'null (map 'list
                                                #'(lambda (x y)
                                                     (unless (zerop y) x))
                                                lst
                                                digits))))
         (loop for ,i from 1 to (1- (expt 2 ,len))
               for ,var = (iterate)
                ,(if collect 'collect 'do) (progn ,@body)))))))

(defmacro do-combinations ((var lst n) &body body)
"Returns all combinations having 'n' elements of list 'lst' in lexicographic order.
    var: Variable to which combinations are bound. Each combination is presented in a list of having 'n' elements.
      n: The number of elements in each combinations 
   body: Form that is evaluated for each combination. 'Return' can be
         used to interrupt the iterations and return a value."
   (loop with vars = (loop for i from 0 to n collect (gensym))
         for j in (butlast (mapcon #'list vars))
         for s = `(let ((,var (mapcar #'car (list ,@(reverse (cdr vars)))))) (progn ,@body))
                 then `(dolist (,(car j) (mapcon #'list (cdr ,(cadr j)))) ,s)
         finally (return `(dolist (,@(last vars) (mapcon #'list ,lst)) ,s))))
      
