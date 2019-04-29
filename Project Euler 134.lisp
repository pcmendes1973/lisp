;;;; Solution for Problem 134 in Project Euler
;;;; Paulo Mendes, 29-APR-2019

(defun circle (&rest lst)
"Returns a circular list with the elements in 'lst'."
   (setf (cdr (last lst)) lst))

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

(defmacro with-gensyms (syms &body body) `(let ,(loop for s in syms collect `(,s (gensym))) ,@body))

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

(pprint
(let ((p1 5) (d 10))
   (do-odd-primes (p2 1000003 :min 7 :action summing)
       (let ((d (if (> d p1) d (setf d (* 10 d))))
             (v (* (- p2 p1) (modular-inverse d p2))))
           (prog1 (+ (* d (rem v p2)) p1) (setf p1 p2))))))
