;;;; Solution for Project Euler Problem 93
;;;; Paulo Mendes, 24-JUN-2016

(defmacro all-expressions (n-symbols op-list)
  (let ((expressions Nil)
        (symbols (loop for i from 1 to n-symbols collect (gensym))))
  (labels
     ((apply-operator (op prev) (cons `(,op ,(car prev) ,(cadr prev)) (cddr prev)))
      (list-rotations (lst)
        (loop for i from 0 below (length lst)
              for j = (copy-seq lst)
              do (rotatef (car j) (car (nthcdr i j)))
              collect j))
      (all-results (seq &optional (previous-results Nil))
        (cond
         ((and (null seq) (= (length previous-results) 1))
          (push (car previous-results) expressions))
         ((null seq)
          (loop for i in op-list
                do (all-results seq (apply-operator i previous-results))))
         ((> (length previous-results) 1)
          (loop for i in op-list
                do (all-results seq (apply-operator i previous-results)))
          (loop for i in (list-rotations seq)
                do (all-results (cdr i) (cons (car i) previous-results))))
         (t
          (loop for i in (list-rotations seq)
                do (all-results (cdr i) (cons (car i) previous-results)))))))
    (all-results symbols)
       `(lambda ,symbols (list ,@expressions)))))

(defparameter m (all-expressions 4 (* + - (lambda (x y) (if (zerop y) 0 (/ x y))))))

(defun longest-consecutive-sequence (lst)
   (let ((maximum 0) (count 0))
      (reduce #'(lambda (x y)
                   (if (= (abs (- x y)) 1)
                       (setf maximum (max maximum (incf count)))
                      (setf count 0))
                   (values y)) lst)
    (values (1+ maximum))))

(reduce #'(lambda (x y) (if (> (cdr x) (cdr y)) x y))
(loop for a from 1 to 6 nconcing
  (loop for b from (1+ a) to 7 nconcing
    (loop for c from (1+ b) to 8 nconcing
       (loop for d from (1+ c) to 9
             collect (cons (list a b c d) (longest-consecutive-sequence
                 (mapcon #'(lambda (x) (if (cdr x)
                                         (and (not (eq (cadr x) (car x))) (list (car x)))
                                        (list (car x))))
                      (sort (remove-if #'(lambda (x)
                                           (or (> (denominator x) 1)
                                               (< x 1))) (funcall m a b c d)) #'<)))))))))
