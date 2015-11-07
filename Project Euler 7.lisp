;;;; Solution for problem 7 in Project Euler
;;;; https://projecteuler.net/problem=7
;;;; Paulo Mendes, 11-OCT-2015

(let* ((sieve '((5 5))) (sieve-end sieve))
  (defun caught-by-elem (elem n)
    (let ((i (car elem)))
      (or (> i n) (setf (car elem) (+ i (cadr elem))))
      (= i n)))

(defun nth-prime (n) 
  (loop for i = 6 then (if (> j 0) (+ i 6) i)
        for j = 1 then (* j -1)
        for k = (+ i j)
        for is-prime = (notany #'(lambda (x) (caught-by-elem x k)) sieve)
        counting is-prime into n-primes
        if is-prime do (setf (cdr sieve-end) `((,k ,k))
                              sieve-end (cdr sieve-end))
        until (= n-primes (- n 2))
        finally (return (cadar (last sieve 2))))))
