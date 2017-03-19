;;;; Solution for problem 315 in Project Euler
;;;; Paulo Mendes, 19-MAR-2017


;;; LEDs lighted by each digit in the following order: left-upper, left-lower, middle-upper
;;; middle-center, middle-lower, right-upper, right-lower
(defparameter LEDs (list #(t t t Nil t t t)
                         #(Nil Nil Nil Nil Nil t t)
                         #(Nil t t t t t Nil)
                         #(Nil Nil t t t t t)
                         #(t Nil Nil t Nil t t)
                         #(t Nil t t t Nil t)
                         #(t t t t t Nil t)
                         #(t Nil t Nil Nil t t)
                         #(t t t t t t t)
                         #(t Nil t t t t t)))

;;; Number of lighted LEDs for digit 'n'
(defparameter n-LEDs (coerce (loop for i from 0 to 9
                                   collect (count t (nth i LEDs)))
                             'array))

(let ((memo (make-array '(10 11) :initial-element Nil)))
(defun count-transitions (d1 &optional d2)
"Returns the number of segments that must change so that
 digit 'd1' becomes digit 'd2'."
   (or (aref memo d1 (if d2 d2 10))
   (setf (aref memo d1 (if d2 d2 10))
    (count t (if d2
                  (map 'vector #'neq (nth d1 LEDs) (nth d2 LEDs))
                (nth d1 LEDs)))))))

(let ((memo (make-hash-table)))
(defun list-digits (n)
"Returns a list of the digits of 'n'"
   (or (gethash n memo)
   (setf (gethash n memo)
   (loop for i = n then (floor i 10)
         until (zerop i)
         collect (mod i 10) into s
         finally (return (reverse s)))))))

(let ((memo (make-hash-table :test #'equal)))
(defun transition-to-next (first next)
"Returns the number of transitions necessary to change from 'first' to 'next', where
 'first' and 'next' are lists of digits."
 (let ((params (nconc (list first) (list next))))
   (or (gethash params memo)
   (setf (gethash params memo)
     (progn 
        (if (> (length next) (length first)) (rotatef next first))
        (map 'vector #'(lambda (x y) (if y (count-transitions x y) (count-transitions x)))
             first 
             (concatenate 'vector
                          (loop repeat (- (length first) (length next))
                                collect Nil)
                          next))))))))

(defun transitions-for-digit-root (n)
"Returns the number of transitions required to show all numbers in the countdown to the digital
 root of 'n' using, in that order, Sam's clock and Max's clock."
   (let* ((steps (loop for i = (list-digits n) then (list-digits (reduce #'+ i))
                       collect i
                       until (= (length i) 1)))
          (transitions (maplist #'(lambda (x) (reduce #'+ (transition-to-next (car x) (cadr x))))
                             (cons Nil steps))))
       (values
        (* 2 (reduce #'+ (mapcar #'(lambda (x) (reduce #'+ (mapcar #'(lambda (y) (aref n-LEDs y)) x)))
                                 steps)))
        (reduce #'+ transitions))))

(defmacro do-primes (var n &rest body)
"Executes 'body' once for each prime number up to n. 'var' is a variable
 that references 'n'"
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

(let ((sum 0))
   (do-primes v 20000000
                 (when (> v 10000000)
                     (incf sum
                           (apply #'- (multiple-value-list (transitions-for-digit-root v))))))
  (values sum))
