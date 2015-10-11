;;;; Solution for problem 7 in Project Euler
;;;; https://projecteuler.net/problem=7
;;;; Paulo Mendes, 11-OCT-2015

(defun nth-prime (prime-to-find)
"Finds nth prime number"
  (let* ((primes '(2))         ; primes: Primes found so far
         (last-prime primes))  ; last-prime: End of the primes list
  (loop for i from 3 by 2
        for is-prime = (notany #'(lambda (x) (= (mod i x) 0)) primes) ; Checks if i is prime
        for n-primes = 2 then (if is-prime (1+ n-primes) n-primes)    ; Updates prime counter
        if is-prime do (setf (cdr last-prime) (cons i Nil)            ; Appends i to primes list as needed
                             last-prime (cdr last-prime))
        until (= n-primes prime-to-find)
        finally (return i))))

(nth-prime 10001)
