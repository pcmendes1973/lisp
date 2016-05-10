;;;; Solution for Problem 205 in Project Euler
;;;; Paulo Mendes, 09-MAY-2016

;; Probability distributions for each die are stored in arrays
(defconstant *pyramid* #(1/4 1/4 1/4 1/4))
(defconstant *cube* #(1/6 1/6 1/6 1/6 1/6 1/6))

(defun multiply-dist (&rest dists)
"Adds up the categorical distributions stored in arrays listed in dist. Returns an
 array containing a  distribution corresponding to the result."
  (if (> (length dists) 2)
     (multiply-dist (car dists)
	            (apply #'multiply-dist (cdr dists)))
   (let ((a (car dists)) (b (cadr dists)))
   (loop with opt = (make-array (+ (length a)
                                   (length b)) :initial-element 0)
         for i from 1 to (length a)
		    do (loop for j from 1 to (length b)
			         do (incf (aref opt (1- (+ i j)))
			                    (* (aref a (1- i))
			                       (aref b (1- j)))))
		 finally (return opt)))))

(defun die-throws (die-dist n-throws)
"Returns the distribution produced by throwing a die n times"
  (apply #'multiply-dist
         (loop for i from 1 to n-throws collect die-dist)))

(float 
  (loop with cube-dist    = (die-throws *cube* 6)
        with pyramid-dist = (die-throws *pyramid* 9)
        for i across cube-dist
        for j from 0
        summing (loop for k from (1+ j) to 35
                      summing (* (aref cube-dist j)
                                 (aref pyramid-dist k)))))
