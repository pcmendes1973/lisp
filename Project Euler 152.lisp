;;;; Solution for problem 152 in Project Euler
;;;; Paulo Mendes, 25-Mar2019

(defun all-combs (lst)
   (if (= (length lst) 1) (cons 0 lst) (mapcan #'(lambda (x) (list x (+ (car lst) x))) (all-combs (cdr lst)))))

(defun largest-factor (n)
    (loop for i in '(79 73 71 67 61 59 53 47 43 41 37 31 29 23 19 17 13 11 7 5 3 2)
             when (zerop (mod (denominator n) i)) return i))

              
(let ((all-fractions (list (loop for i from 2 to 80 summing (expt i -2))))
      (fishbone (loop with primes = (mapcar #'list '(79 73 71 67 61 59 53 47 43 41 37 31 29 23 19 17 13 11 7 5 3 2))
                      for i from 2 to 80 
                      for item = (expt i -2)
                        do (push item (cdr (find (largest-factor item) primes :key 'car)))
                      finally (return (mapcar #'(lambda (x) (list (car x) (sort (all-combs (cdr x))  #'<))) primes)))))

  (defun test-all-sums (lst1 lst2 factor lower-limit)
     (loop for i in lst1 nconc
       (loop for j in lst2 for k = (- i j)
             until (> lower-limit k)
             when (> factor (largest-factor k))
               collect k)))

 (loop for i in (butlast fishbone)
       for j = (test-all-sums all-fractions (cadr i) (car i) 1/2)
           then (test-all-sums j (cadr i) (car i) 1/2)
       do (pprint j)
       finally (return (loop for k in j
                     counting (find k (mapcar #'(lambda (x) (+ 1/2 x)) (cadar (last fishbone))))))))
