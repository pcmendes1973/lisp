;;;; Solution for problem 119 in Project Euler
;;;; Paulo Mendes, 30-MAY-2017


(defun sum-digits (n)
   (if (zerop n) 0
      (multiple-value-bind (mod rest)
         (floor n 10)
            (+ (sum-digits mod) rest))))

(defun is-a? (n)
   (loop for i from 2 to 20
         for num = (expt n 2) then (* num n)
         when (= (sum-digits num) n)
           collect num))

(nth 29 (sort (loop for i from 2 to 100
                    when (is-a? i)
                      nconc it)
              #'<))
