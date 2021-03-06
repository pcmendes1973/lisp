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
   body: Form that is evaluated for each value of 'n'. 'Return' can be used
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
         used to interrupt the iterations and return a value.
 RETURN VALUES
   Nil"
   (loop with vars = (loop for i from 0 to n collect (gensym))
         for j in (butlast (mapcon #'list vars))
         for s = `(let ((,var (mapcar #'car (list ,@(reverse (cdr vars)))))) (progn ,@body))
                 then `(dolist (,(car j) (mapcon #'list (cdr ,(cadr j)))) ,s)
         finally (return `(dolist (,@(last vars) (mapcon #'list ,lst)) ,s))))

(defun gray (n k)
"Returns the kth item of the n-bit Gray code sequence. Output is a bit array."
  (assert (< k (expt 2 n)) (n) "k cannot be higher than ~A~%" (1- (expt 2 n)))
  (cond
    ((= n 1)
      (if (zerop k) #*0 #*1))
    ((>= k (expt 2 (1- n)))
      (concatenate 'bit-vector #*1 (gray (1- n) (- (expt 2 n) k 1))))
    (t
      (concatenate 'bit-vector #*0 (gray (1- n) k)))))


(defmacro do-odd-primes ((var max &key (min 3) (action 'do)) &body body)
 "Iterates over all odd primes between min and max (inclusive). Primes are calculated using the Sieve of
 Sundaram over a bit array.
    var: Variable to which odd primes are bound.
    max: Limit to prime number list.
 action: Chooses the keyword used by the internal loop to process results yielded by the form 'body' (see below).
         Possible values are 'summing', 'appending', 'nconc', 'maximizing' and 'minimizing' (i.e., same keywords
         as in the 'loop' macro).
   body: Form that is evaluated for each value of 'var'.
    RETURN VALUES
      Depends on 'action' and 'body'. These items are processed as in the 'loop' macro."
  (with-gensyms (imin imax n m i j pos buffer)
    `(let* ((,imax ,max) (,imin (max 3 ,min)) (,n (floor (1- ,imax) 2)) (,m (floor (1- ,imin) 2)))
       (let ((,buffer (make-array (list (- ,n ,m -1)) :element-type 'bit :initial-element 0)))
         (loop for ,i from 1 to (floor (1- (isqrt ,imax)) 2)
            do (loop for ,j from (max ,i (ceiling (- ,m ,i) (1+ (* 2 ,i))))
                  for ,pos = (- (+ ,i ,j (* 2 ,i ,j)) ,m)
                  until (> ,pos (- ,n ,m)) do (setf (aref ,buffer ,pos) 1))
            finally (return (loop for ,i across ,buffer for ,j from ,m
                                  when (zerop ,i) ,action (let ((,var (1+ (* 2 ,j)))) (progn ,@body)))))))))

(defun expt-mod (a b mod)
"Returns a ^ b mod 'mod'"
  (if (zerop b) 1
    (multiple-value-bind (div rem)
       (floor b 2)
         (let ((m (expt-mod a div mod)))
           (mod (* (mod (expt m 2) mod) (if (zerop rem) 1 a)) mod)))))

(defun circle (&rest lst)
"Returns a circular list with the elements in 'lst'."
   (setf (cdr (last lst)) lst))

(defun bezout-coefficients (a b)
"Returns the Bézout coefficients of a and b and gcd (a,b)"
  (macrolet ((pcircle (n circle)
                (let ((c (gensym)))
                 `(let ((,c ,circle))
                    (setf (cadr ,c) ,n circle (cdr ,c))))))
  (do* ((q (floor b a) (floor (cadr r) (car r)))
        (r (circle b a)
              (pcircle (- (cadr r) (* q (car r))) r))
        (s (circle 0 1)                                     
              (pcircle (- (cadr s) (* q (car s))) s))
        (tau (circle 1 0)                                   
              (pcircle (- (cadr tau) (* q (car tau))) tau)))
    ((zerop (car r)) (values (cadr s) (cadr tau) (cadr r))))))


(defun modular-inverse (a b)
"Returns the modular inverse of a mod b. If a and b are not coprime or no
 inverse exists, return Nil"
  (macrolet ((pcircle (n circle)
                (let ((c (gensym)))
                 `(let ((,c ,circle))
                    (setf (cadr ,c) ,n circle (cdr ,c))))))
  (do* ((q (floor b a) (floor (cadr r) (car r)))
        (r (circle b a)
              (pcircle (- (cadr r) (* q (car r))) r))
        (s (circle 0 1)                                     
              (pcircle (- (cadr s) (* q (car s))) s)))
    ((zerop (car r)) (cond
                        ((> (cadr r) 1) Nil)
                        ((zerop (cadr s)) Nil)
                        ((< (cadr s) 0) (+ b (cadr s)))
                        (t (cadr s)))))))

(defmacro operatorf (operand1 operand2 operator)
  "Executes operand1 <- operand1 <operator> operand2 and returns operand1"
  (let* ((sym (gensym)) (sym operand1))
    `(setq ,sym (,operator ,sym ,operand2))))

(defmacro do-all-sequences ((var lst op init) &body body)
"Iterate over all combinations of elements in 'lst'. In each iteration,
 'var' is set to init <op> lst1 <op> lst1 <op> lst3, where 'init' is the initial
 element, lst(n) is an element in 'lst', and 'op' is an arbitrary operator.
 Returns Nil"
  (let ((n (gensym)) (l (gensym)))
   `(labels ((rep (,l ,n)
               (if ,l (progn
                        (rep (cdr ,l) (,op ,n (car ,l)))
                        (rep (cdr ,l) ,n))
                   (let ((,var ,n)) (progn ,@body)))))
      (rep ,lst ,init))))
