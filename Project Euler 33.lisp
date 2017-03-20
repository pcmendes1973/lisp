;;;; Solution for Problem 33 in Project Euler
;;;; Paulo Mendes, 19-MAR-2017

(defmacro do-permutations ((var vec &key append-into (len var ext-len)) &body body)
  (let ((n (gensym)))
  `(block Nil
    (let ((,var ,vec) ,@(if append-into `((,append-into Nil))))
     (labels ((generate (&optional (,n ,(if ext-len len `(length ,var))))
                (if (= ,n 1)
                    ,(if append-into
                         `(setf, append-into
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

(defun combinations (lst n)
    (cond
       ((= (length lst) n) (list lst))
       ((< (length lst) n) Nil)
       ((= n 1) (mapcar #'list (copy-seq lst)))
       ((null lst) Nil)
       ((zerop n) Nil)
       (t
         (nconc (combinations (cdr lst) n)
                (mapcar #'(lambda (x) (cons (car lst) x))
                        (combinations (cdr lst) (1- n)))))))

(let ((fractions Nil))
    (mapcar #'(lambda (x)
       (do-permutations (v (copy-seq x))
           (let ((fraction1 (/ (+ (* 10 (nth 0 v)) (nth 1 v))
                               (+ (* 10 (nth 1 v)) (nth 2 v))))
                 (fraction2 (/ (nth 0 v) (nth 2 v))))
               (when (= fraction1 fraction2)
                 (pushnew fraction1 fractions)))))
            (combinations '(1 2 3 4 5 6 7 8 9) 3))
(reduce #'* fractions))
