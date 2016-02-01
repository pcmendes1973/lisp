;;;; Solution for project euler problem 172
;;;; Paulo Mendes, 1-FEB-2016
;;;; Solves the recursion below
;;;; $N (n, d, r, f) = 
;;;;    X=
;;;;    \begin{cases}
;;;;      d & \text{if}\ n = 1 \\
;;;;      0 & \text{if}\ n > d \cdot r \\
;;;;      (d - f) \cdot N (n - 1, d, r, f) & \text{if}\ r > n\\
;;;;      (d - f) \cdot  \sum\limits_{k=1}^r {n - 1 \choose k - 1} \cdot N (n - k, d - 1, r, 0)  & \text{otherwise}\\
;;;;    \end{cases}$


(defun choose (n k)
   (loop for i from 0 to k
         for j = 1 then (* j (/ (- n i -1) i))
         finally (return j)))

(defun count-numbers (n d r f)
   (cond 
     ((= n 1) d)
     ((> n (* d r)) 0)
     ((>= r n) (* (- d f) (count-numbers (1- n) d r 0)))
     (t (loop for i from 1 to r
              summing (* (choose (1- n) (1- i))
                      (count-numbers (- n i) (1- d) r 0)) into s
              finally (return (* (- d f) s))))))
