;;;; Solution for Problem 60 in Project Euler
;;;; Paulo Mendes, 21-JUN-2017

(defmacro do-primes (var n &rest body)
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


(let ((multiples '(1)))
   (defun concatenate-numbers (m n)
      (labels ((multiplier (num &optional (mul multiples))
                  (or (find-if #'(lambda (x) (> x num)) multiples)
                      (multiplier num (setf multiples
                                            (append multiples
                                                    (list (* (car (last multiples)) 10))))))))
      (+ (* m (multiplier n)) n))))

(defun is-prime? (n &optional (prime-list '(3 2)))
   (and (notany #'(lambda (x) (and (> n x) (zerop (mod n x)))) prime-list)
        (loop for i from (+ (car prime-list) 2) to (1+ (isqrt n)) by 2
              never (zerop (mod n i)))))

(defun insert-prime! (n prime-list)
  (if (loop for i in prime-list
            always (and (is-prime? (concatenate-numbers n i))
                        (is-prime? (concatenate-numbers i n))))
     (prog1 prime-list
            (setf (cdr prime-list)
                  (cons n (cdr prime-list))))))

(let ((primes-list))
  (defun test-prime (p)    
    (push (list p) primes-list)
    (values (mapcan #'(lambda (x &aux (v (insert-prime! p x)))
                          (if v (progn
                                  (if v (push x primes-list))
                                  (list v))))
                    primes-list)
            primes-list)))
  
(defconstant *bignum* 100000000)

(let ((five-seq (list *bignum*)) (four-seq (list *bignum*)) (primes Nil))
   (do-primes v 20000
     (push v primes)
     (multiple-value-bind (finds) (test-prime v)
       (loop for i in finds
             when (= (length i) 4)
               do (if (> (reduce #'+ four-seq)
                         (reduce #'+ i))
                      (setf four-seq i))
             when (= (length i) 5)
               do (if (> (reduce #'+ five-seq)
                         (reduce #'+ i))
                      (setf five-seq i)))))
   (values four-seq five-seq))
