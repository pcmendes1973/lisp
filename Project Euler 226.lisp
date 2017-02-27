;;;; Solution for Problem 226 in Project Euler
;;;; Paulo Mendes, 27-FEB-2017


(defun blancmange (x)
"Returns the value of the Takagi (blancmange) function for x"
    (flet ((s (x)
             (let ((mod (cadr (multiple-value-list (floor x)))))
                   (if (> mod 0.5) (- 1 mod) mod))))
    (loop for n from 0
          for exponent = 1.0d0 then (* exponent 2)
          for term = (s x) then (/ (s (* exponent x)) exponent)
          summing term into result
          until (zerop term)
          finally (return result))))

(defun distance-from-center (x)
"Returns the distance from blancmange (x) to the point at (1/4, 1/2)"
  (sqrt 
     (+ (expt (- x 0.25d0) 2)
        (expt (- (blancmange x) 0.5d0) 2))))


(defun blancmange-integral (x)
"Returns the area under the blancmange curve for [0,x]"
   (cond 
      ((zerop x) 0d0)
      ((<= 0 x 1/2) (+ (/ (blancmange-integral (* 2 x)) 4)
                       (/ (expt x 2) 2)))
      ((<= 1/2 x 1) (- 0.5d0
                       (blancmange-integral (- 1 x))))
      (t
         (let ((n (floor x)))
           (+ (/ n 2)
              (blancmange-integral (- x n)))))))

(defmacro Dekker (func (a b) (a0 b0) &body body)
   "Dekker's minimization method. Finds the minimum of func in interval 
    [a0, b0]. Each iteration pushes a new set of value for [a, b] into lists
    a and b. Returns the first result of body that evaluates to non-Nil."
  (with-gensyms (s m previous-step f)
     `(let* ((,b (list ,b0 ,a0))
             (,a (cdr ,b))
             (,previous-step Nil)
             (,f ,func))
         (labels ((,m () (setf ,previous-step 'bisection)
                         (/ (+ (car ,a) (car ,b)) 2.0D0))
                  (,s () 
                  (let ((bk (car ,b)) (bk-1 (cadr ,b)))
                     (if (not (= (funcall ,f bk) (funcall ,f bk-1)))
                         (progn
                           (setf ,previous-step 'secant)
                           (- bk
                             (* (funcall ,f bk)
                                (/ (- bk bk-1)
                                   (- (funcall ,f bk)
                                      (funcall ,f bk-1))))))
                       (values (,m))))))
           (loop when (progn ,@body)
                   return it
                 do (let ((secant (,s)) (mean (,m)))
                         (if (> (car ,b) secant mean) ;Pushes bk+1 into list b
                              (push secant ,b)        ;
                            (push mean ,b))           ;
                         (if (= (signum (funcall ,f (car ,a)))
                                (signum (funcall ,f (car ,b))))
                              (push (cadr ,b) ,a)     ; Same signs,     ak+1 = bk
                             (push (car ,a) ,a))      ; Opposite signs, ak+1 = ak
                         (if (< (abs (funcall ,f (car ,a)))
                                (abs (funcall ,f (car ,b))))
                              (rotatef (car ,a) (car ,b)))))))))

;; Apply Dekker's method to find x for the 
;; intersection and calculate the area therefrom
;; 0.122310730724739452785d0 is the area below 
;; the circle, courtesy of Wolfram Alpha
(Dekker #'(lambda (x)
              (- (distance-from-center x) 0.25d0))
          (a b)
          (0.05 0.1)
          (let ((da (distance-from-center (car a)))
                (db (distance-from-center (car b))))
               (and (< (abs (- db da))
                       1d-15)
                       (- (blancmange-integral 0.5d0)     
                          (blancmange-integral (car b))   
                           0.122310730724739452785d0))))  
