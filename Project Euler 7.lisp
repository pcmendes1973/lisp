;;;; Solution for problem 7 in Project Euler
;;;; https://projecteuler.net/problem=7
;;;; Paulo Mendes, 11-OCT-2015

(defun nth-prime (prime-to-find)
"Finds nth prime number"
  (if (< prime-to-find 3) (1+ prime-to-find))
  (let* ((primes '(2 3))         ; primes: Primes found so far
         (last-prime primes))    ; last-prime: End of the primes list
  (loop for i = 6 then (if (> j 0) (+ i 6) i)
        and j = -1 then (* j -1)
        for k = (+ i j)
        for is-prime = (notany #'(lambda (x) (= (mod k x) 0)) primes) ; Checks if k is prime
        for n-primes = 3 then (if is-prime (1+ n-primes) n-primes)    ; Updates prime counter if it is
        if is-prime do (setf (cdr last-prime) (cons k Nil)            ; Appends k to primes list as needed
                             last-prime (cdr last-prime))
        until (= n-primes prime-to-find)
        finally (return i))))

(nth-prime 10001)
